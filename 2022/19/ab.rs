#![feature(let_chains)]
#![feature(impl_trait_projections)]
#![feature(result_option_inspect)]

use std::collections::{BinaryHeap, HashMap, HashSet};
use std::cmp::{Ordering, PartialOrd, Ord, Reverse};
use std::error::Error;
use std::ops::{Add, Mul, MulAssign, Sub, AddAssign, SubAssign};
use std::str::FromStr;


static mut GLOBAL_ID: usize = 0;

fn maybe<T>(case: bool, a: T) -> Option<T> {
    if case {Some(a)} else {None}
}

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, std::hash::Hash)]
enum Material {Ore, Clay, Obsidian, Geode}
use Material::*;

impl Material {
    fn iter() -> impl Iterator<Item=Self> {
        static VALUES: [Material; 4] = [Ore, Clay, Obsidian, Geode];
        VALUES.iter().copied()
    }
}

type Robot = Material;

impl FromStr for Material {
    type Err = Box<dyn Error>;
    fn from_str(s: &str) -> Result<Material, <Material as FromStr>::Err> {
        match &s.to_ascii_lowercase()[..] {
            "ore" => Ok(Ore),
            "clay" => Ok(Clay),
            "obsidian" => Ok(Obsidian),
            "geode" => Ok(Geode),
            _ => Err(format!("not a valid material: {}", s).into())
        }
    }
}

#[derive(Debug, Default, PartialEq, Eq, Clone)]
struct Amount {
    ore: isize,
    clay: isize,
    obsidian: isize,
    geode: isize
}

impl Add<&Amount> for Amount {
    type Output = Self;
    fn add(mut self, other: &Self) -> Self::Output {self += other; self}
}
impl AddAssign<&Amount> for Amount {
    fn add_assign(&mut self, other: &Self) {
        self.ore += other.ore;
        self.clay += other.clay;
        self.obsidian += other.obsidian;
        self.geode += other.geode;
    }
}
impl AddAssign for Amount {
    fn add_assign(&mut self, other: Self) {*self += &other;}
}
impl SubAssign<&Amount> for Amount {
    fn sub_assign(&mut self, other: &Self) {
        self.ore -= other.ore;
        self.clay -= other.clay;
        self.obsidian -= other.obsidian;
        self.geode -= other.geode;
    }
}

impl Mul<usize> for Amount {
    type Output = Amount; fn mul(mut self, x: usize) -> Self::Output {self *= x; self}
}
impl Mul<usize> for &Amount {
    type Output = Amount; fn mul(self, x: usize) -> Self::Output {self.clone() * x}
}
impl MulAssign<usize> for Amount {
    fn mul_assign(&mut self, x: usize) {
        self.ore *= x as isize;
        self.clay *= x as isize;
        self.obsidian *= x as isize;
        self.geode *= x as isize;
    }
}

impl Amount {
    fn get(&self, m: Material) -> isize {
        match m {
            Ore => self.ore,
            Clay => self.clay,
            Obsidian => self.obsidian,
            Geode => self.geode
        }
    }

    fn get_mut(&mut self, m: Material) -> &mut isize {
        match m {
            Ore => &mut self.ore,
            Clay => &mut self.clay,
            Obsidian => &mut self.obsidian,
            Geode => &mut self.geode
        }
    }
}

#[derive(Debug, Clone)]
struct Production (Amount);

impl Production {
    fn initial() -> Self {
        Production(Amount { ore: 1, clay: 0, obsidian: 0, geode: 0})
    }

    fn increase(mut self, m: Material) -> Self {
        *self.0.get_mut(m)  += 1;
        self
    }

    fn amount(&self, m: Material) -> usize {
        self.0.get(m) as usize
    }
}

#[derive(Debug, Clone)]
struct Recipe (Amount);

impl FromStr for Recipe {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, <Self as FromStr>::Err> {
        let words: Vec<&str> = s.split_ascii_whitespace().collect();

        let mut amount: Amount = Default::default();
        amount.ore = words[4].parse()?;
        if words.len() > 6 {
            *amount.get_mut(words[8].parse()?) = words[7].parse()?
        }
        Ok(Recipe(amount))
    }
}

impl Recipe {
    fn easy(&self) -> Self {
        let mut amount = self.0.clone();
        amount.ore = 0;
        Recipe(amount)
    }

    fn cost(&self, m: Material) -> usize {
        self.0.get(m) as usize
    }
}

#[derive(Debug, Clone)]
struct Stockpile (Amount);

impl Stockpile {
    fn initial() -> Self {
        Stockpile(Default::default())
    }

    fn can_afford(&self, recipe: &Recipe) -> bool {
        self.0.ore >= recipe.0.ore &&
            self.0.clay >= recipe.0.clay &&
            self.0.obsidian >= recipe.0.obsidian
    }

    fn amount(&self, m: Material) -> usize {
        self.0.get(m) as usize
    }

    fn accumulate(mut self, production: &Production, t: usize) -> Stockpile {
        self.0 += &production.0 * t;
        self
    }
}

impl Sub<&Recipe> for Stockpile {
    type Output = Stockpile;
    fn sub(mut self, recipe: &Recipe) -> Self::Output {
        self.0 -= &recipe.0;
        self
    }
}

#[derive(Debug, Clone)]
struct Blueprint (HashMap<Robot, Recipe>);

impl FromStr for Blueprint {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, <Self as FromStr>::Err> {
        if !s.starts_with("Blueprint") { return Err("not a blueprint".into()) }

        let (_, s) = s.split_once(':').unwrap();
        let mut bp = HashMap::new();
        for recipe in s.split_terminator('.') {
            let robot = recipe.split_ascii_whitespace().nth(1)
                .ok_or("not enough words")?;
            bp.insert(robot.parse()?, recipe.parse()?);
        }
        Ok(Blueprint(bp))
    }
}

impl Blueprint {
    fn easy(&self) -> Self {
        Blueprint(self.0.iter().map(|(robot, recipe)| (*robot, recipe.easy())).collect())
    }

    fn recipe(&self, robot: Robot) -> &Recipe {
        self.0.get(&robot).unwrap()
    }

    fn recipes(&self) -> impl Iterator<Item=(Robot, &Recipe)> {
        self.0.iter().map(|(m, r)| (*m, r))
    }

    fn solve(&self, end_time: usize) -> usize {
        let mut frontier: BinaryHeap<Node> = BinaryHeap::new();
        frontier.push(State::initial(&self, end_time).into());

        let mut best = 0;
        let mut best_min = 0;

        while let Some(node) = frontier.pop() {
            let moves = node.state.moves();

            if moves.is_empty() {
                best = best.max(node.state.final_geodes())
            }

            for (_, branch) in moves {
                let max = branch.upper_bound();
                if max < best_min || max < best{ continue }

                let node = Node::new(branch.lower_bound(), max, branch);
                best_min = best_min.max(node.min);

                if node.min == node.max {
                    best = best.max(node.min)
                } else if node.max > 0 {
                    frontier.push(node);
                }
            }
        }
        best
    }
}

#[derive(Clone)]
struct State<'a> {
    blueprint: &'a Blueprint,
    stockpile: Stockpile,
    production: Production,
    time: usize,
    end_time: usize,
    id: usize
}

impl PartialEq for State<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Eq for State<'_> {}

impl std::fmt::Debug for State<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("State")
            .field("time", &self.time)
            .field("stockpile", &self.stockpile)
            .field("production", &self.production)
            .finish()
    }
}

impl State<'_> {
    fn new<'a>(blueprint: &'a Blueprint, stockpile: Stockpile, production: Production, time: usize, end_time: usize)
    -> State<'a> {
        let id = unsafe {GLOBAL_ID += 1; GLOBAL_ID};
        State {blueprint, stockpile, production, time, end_time, id}
    }

    fn initial<'a>(blueprint: &'a Blueprint, end_time: usize) -> State<'a> {
        State::new(blueprint, Stockpile::initial(), Production::initial(), 0, end_time)
    }

    fn moves(&self) -> Vec<(Material, Self)> {
        Material::iter().filter_map(|m| self.build_next(m).map(|s| (m, s))).collect()
    }

    fn final_geodes(self) -> usize {
        self.stockpile.accumulate(&self.production, self.end_time - self.time).amount(Geode)
    }

    fn worth_building(&self, robot: Robot) -> bool {
        // If we're already producing more than we can use, then we shouldn't
        // make more.
        // We could also work out if there is no possibility of building the
        // target in the remaining time, but this feels like it would be a
        // pretty marginal gain.
        robot == Geode ||
            self.blueprint.recipes()
            .filter(|(target, _)| *target != robot)
            .any(|(_, recipe)| recipe.cost(robot) > self.production.amount(robot))
    }

    fn time_to_build(&self, recipe: &Recipe) -> Option<usize> {
        // Time until we can next build robot m.
        // If we aren't producing a requisite material, then we can't make it
        // and time will be None.
        // We also can't exceed end_time, and building a robot in the last time
        // step is pointless.
        let mut time = 0;
        for m in Material::iter() {
            let n = recipe.cost(m);
            if n == 0 { continue }

            let dn = self.production.amount(m);
            if dn == 0 { return None }
            let n = n.saturating_sub(self.stockpile.amount(m)) as isize;

            if n == 0 {
                time = time.max(1)
            } else {
                // +2 here: +1 for time step *after* satisfying need
                //          +1 to correct pre-division -1
                time = time.max((2 + (n - 1) / dn as isize) as usize)
            }
        }
        maybe(self.time + time < self.end_time, time)
    }

    fn build_next(&self, robot: Robot) -> Option<Self> {
        if !self.worth_building(robot) {return None}

        let recipe = self.blueprint.recipe(robot);

        self.time_to_build(&recipe).map(|time| {
            State::new(
                &self.blueprint,
                self.stockpile.clone().accumulate(&self.production, time) - &recipe,
                self.production.clone().increase(robot),
                self.time + time,
                self.end_time)
        })
    }

    fn lower_bound(&self) -> usize {
        // Our lower bound is the path of only building geode robots from now on.
        // This is a bit wasteful as we may also search this branch
        // - but I can't be bothered to populate visited states
        std::iter::successors(Some(self.clone()), |state: &Self| state.build_next(Geode))
            .last()
            .map(|state| state.final_geodes())
            .unwrap_or(0)
    }

    fn upper_bound(&self) -> usize {
        // To calculate an upper bound on geode production, we solve a branchless
        // subproblem in which ore costs are 0 and we can create one robot of any
        // type each minute.

        let bp = self.blueprint.easy();
        let obsidian_cost = bp.recipe(Obsidian).cost(Clay);
        let geode_cost = bp.recipe(Geode).cost(Obsidian);
        let mut stockpile = self.stockpile.clone();
        let mut production = self.production.clone();

        for _ in self.time..=self.end_time {
            // find robots you can build
            let obsidian = stockpile.0.clay > obsidian_cost as isize;
            let geode = stockpile.0.obsidian > geode_cost as isize;

            // collect resources from built robots
            stockpile = stockpile.accumulate(&production, 1);

            // build robots
            if obsidian {
                stockpile.0.clay -= obsidian_cost as isize;
                production.0.obsidian += 1;
            }
            if geode {
                stockpile.0.obsidian -= geode_cost as isize;
                production.0.geode += 1;
            }
            production.0.clay += 1
        }
        stockpile.amount(Geode)
    }
}

#[derive(Debug, Eq)]
struct Node<'a> {
    state: State<'a>,
    min: usize,
    max: usize
}

impl Node<'_> {
    fn new<'a>(min: usize, max: usize, state: State<'a>) -> Node<'a> {
        Node {state, min, max}
    }

    fn key(&self) -> impl Ord {
        // A few options here, this is what I found worked best.
        // Including Reverse(time) makes this approximately breadth first.
        //(self.min, self.max)
        //(self.min, self.max, Reverse(self.state.time))
        //(self.state.production.amount(Geode), Reverse(self.state.time))
        (self.state.production.amount(Geode), self.max, Reverse(self.state.time))
    }
}

impl<'a> From<State<'a>> for Node<'a> {
    fn from(state: State<'a>) -> Node<'a> {
        Node::new(state.lower_bound(), state.upper_bound(), state)
    }
}
impl PartialOrd for Node<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Node<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.key().cmp(&other.key())
    }

}

impl PartialEq for Node<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.state.eq(&other.state)
    }
}


fn main() {
    let blueprints: Vec<Blueprint> = std::io::stdin()
        .lines()
        .flat_map(|l| l)
        .map(|l| l.parse::<Blueprint>().unwrap())
        .collect();

    println!("{}", blueprints
             .iter()
             .enumerate()
             .fold(0, |n, (i, bp)| n + (1+i) * bp.solve(24)));
    println!("{}", blueprints
             .iter()
             .take(3)
             .map(|bp| bp.solve(32))
             .product::<usize>()
    )
}


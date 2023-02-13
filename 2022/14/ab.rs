use std::ops::{Add, RangeInclusive};
use std::collections::{HashMap};

#[derive(PartialEq, Eq)]
enum Tile {
    Sand,
    Brick
}
use Tile::*;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
struct Coord(isize, isize);

const DIRS: [Coord; 3] = [Coord(0, 1), Coord(-1, 1), Coord(1, 1)];
const START: Coord = Coord(500, 0);

impl Add for &Coord {
    type Output = Coord;
    fn add(self, other: Self) -> <Self as Add>::Output {
        Coord(self.0 + other.0, self.1 + other.1)
    }
}

type Map = HashMap::<Coord, Tile>;

fn parse_line(l: &str) -> Result<Vec<Coord>, String>  {
    l.split(" -> ").map(|coord| {
        let (x, y) = coord.split_once(',').unwrap();
        x.parse().and_then(|x| y.parse().map(|y| Coord(x, y)))
            .or(Err(format!("Could not parse {}", coord)))
    }).collect()
}

fn next_position(map: &Map, point: &Coord) -> Option<Coord> {
    DIRS.iter().map(|dir| point + dir).filter(|p| !map.contains_key(p)).next()
}

fn ordered_range(x: isize, y: isize) -> RangeInclusive<isize> {
    if x < y {x..=y} else {y..=x}
}

fn run(mut map: &mut Map, mut active_sand: &mut Vec<Coord>, bottom: isize, end_condition: impl Fn(Coord) -> bool) {
    fn come_to_rest(map: &mut Map, pending: &mut Vec<Coord>) {
        map.insert(pending.pop().unwrap(), Sand);
    }

    while let Some(current) = active_sand.last() {
        match next_position(&map, &current) {
            Some(end_point) if end_condition(end_point) => break,
            Some(floor) if floor.1 == bottom+2
                => come_to_rest(&mut map, &mut active_sand),
            Some(next) => {active_sand.push(next); }
            None => come_to_rest(&mut map, &mut active_sand)
        }
    }
}

fn count_sand(map: &Map) -> usize {
    map.values().filter(|&x| x == &Sand).count()
}

fn main() {
    let mut map: Map = HashMap::new();
    for l in std::io::stdin().lines() {
        parse_line(&l.unwrap()).unwrap()
            .into_iter()
            .reduce(|Coord(x0, y0), next@Coord(x1, y1)| {
                for x in ordered_range(x0, x1) {
                    for y in ordered_range(y0, y1) {
                        map.insert(Coord(x, y), Brick);
                    }
                }
                next
            });
    }
    let bottom = map.keys().map(|x| x.1).max().unwrap();
    let mut active_sand = Vec::from([START]);
    run(&mut map, &mut active_sand, bottom, |p| p.1 >= bottom);
    println!("{}", count_sand(&map));

    run(&mut map, &mut active_sand, bottom, |p| p == START);
    println!("{}", count_sand(&map));
}

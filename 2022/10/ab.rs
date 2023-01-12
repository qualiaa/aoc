use std::str::FromStr;

type State = (usize, isize);

#[derive(Debug)]
enum Instruction {
    NoOp,
    AddX(isize)
}
use Instruction::*;

impl FromStr for Instruction {
    type Err = Box<dyn std::error::Error>;
    fn from_str(s: &str) -> Result<Self, <Self as FromStr>::Err> {
        let parts: Vec<_> = s.trim().split(' ').collect();

        match parts.get(0) {
            Some(&"noop") => Ok(NoOp),
            Some(&"addx") => {
                let x = parts.get(1).ok_or("missing x")?;
                x.parse().map(AddX).map_err(|e| e.into())
            }
            _ => Err("missing instruction").map_err(|e| e.into())
        }
    }
}

fn signal_strength(cycle: usize, x: isize) -> isize {
    x * cycle as isize
}

fn execute(insts: impl Iterator<Item=Instruction>) -> Vec<State> {
    // Originally implemented this with scan/iterator nonsense, but it was too
    // ugly even for my eyes... We create a list pairing each value of x with
    // the last cycle to hold that value.
    let mut lx = 1;
    let mut cycle = 1;
    let mut result = Vec::new();
    for inst in insts {
        match inst {
            NoOp => cycle += 1,
            AddX(dx) => {
                cycle += 2;
                result.push((cycle-1, lx));
                lx += dx;
            }
        }
    }
    // Annoying MAX can't resolve polymorphically... (because it's a value, not
    // an ad-hoc function as it would be in Haskell)
    result.push((std::usize::MAX, lx));
    result
}

fn option<T>(p: bool, v: T) -> Option<T> {
    if p {Some(v)} else {None}
}

fn main() {
    let states = execute(
        std::io::stdin().lines()
            .map(|l| l.unwrap())
            .map(|l| l.parse().unwrap()));

    let mut targets = IntoIterator::into_iter([20,60,100,140,180,220]);
    println!("{:?}", states.iter().scan(
        (targets.next(), 1), |(t, lx), &(cycle, x)| {
            t.map(|target| {
                *lx = x;
                option(target <= cycle, x)
                    .map(|x| {*t = targets.next(); signal_strength(target, x)})
            })
        }).filter_map(std::convert::identity).sum::<isize>());

    let mut states = states.into_iter();
    let mut state = states.next();
    let pixels = (1..=240).map(|i| {
        let (cycle, x) = state.unwrap();
        if i == cycle {state = states.next();}
        (((i-1) % 40) as isize - x).abs() < 2
    });

    for (i, pixel) in pixels.enumerate(){
        print!("{}", if pixel {'%'} else {' '});
        if i % 40 == 39 {
            println!("");
        }
    }
}

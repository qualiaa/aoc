use std::{str::FromStr, convert::TryFrom};

#[derive(Clone, Copy)]
enum Play {Rock, Paper, Scissors}
use Play::*;
#[derive(Clone, Copy)]
enum Outcome {Draw, Win, Lose}
use Outcome::*;

impl FromStr for Play {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.chars().next().and_then(|x| match x {
            'A' => Some(Rock),
            'B' => Some(Paper),
            'C' => Some(Scissors),
            _ => None
        }).ok_or(())
    }
}

impl TryFrom<u32> for Play {
    type Error = ();
    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Rock),
            1 => Ok(Paper),
            2 => Ok(Scissors),
            _ => Err(())
        }
    }
}

impl Play {
    fn from_outcome(outcome: &Outcome, against: &Play) -> Play {
        Play::try_from((*against as u32 + *outcome as u32) % 3).unwrap()
    }
}

impl FromStr for Outcome {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.chars().next().and_then(|x| match x {
            'X' => Some(Lose),
            'Y' => Some(Draw),
            'Z' => Some(Win),
            _ => None
        }).ok_or(())
    }
}

fn score(them: &Play, me: &Play) -> u32 {
    let me = *me as u32;
    let them = *them as u32;
    1 + me + 3*((1 + me + 2 * them) % 3)
}

fn main() {
    let total: u32 = std::io::stdin().lines()
        .map(|l| l.unwrap())
        .map(|l| {
            let them = &l[..1].parse().unwrap();
            let outcome = &l[2..].parse().unwrap();
            score(&them, &Play::from_outcome(&outcome, &them))
        }).sum();
    println!("{}", total);
}

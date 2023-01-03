use std::str::FromStr;

#[derive(Clone, Copy)]
enum Play {Rock, Paper, Scissors}
use Play::*;

impl FromStr for Play {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.chars().next().and_then(|x| match x {
            'A' | 'X' => Some(Rock),
            'B' | 'Y' => Some(Paper),
            'C' | 'Z' => Some(Scissors),
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
        .map(|l| score(&l[..1].parse().unwrap(),
                       &l[2..].parse().unwrap()))
        .sum();
    println!("{}", total);
}

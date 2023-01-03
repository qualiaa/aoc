use std::collections::HashSet;
use std::convert::TryFrom;
use std::iter::FromIterator;

fn priority(c: char) -> Option<u8> {
    u8::try_from(c).ok().and_then(|ord| match c {
        'A'..='Z' => Some(ord - 64 + 26),
        'a'..='z' => Some(ord - 96),
        _ => None
    })
}

fn find_duplicate(left: &str, right: &str) -> Option<char> {
    let left: HashSet<char> = HashSet::from_iter(left.chars());
    for c in right.chars() {
        if left.contains(&c) {
            return Some(c);
        }
    }
    None
}

fn main() {
    let result = std::io::stdin().lines()
        .map(|l| l.unwrap())
        .map(|l| {
            let n = l.len();
            find_duplicate(&l[..n/2], &l[n/2..]).and_then(priority).unwrap() as u32
        }).sum::<u32>();
    println!("{:?}", result)
}

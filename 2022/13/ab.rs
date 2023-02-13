use std::cmp::Ordering::{self,*};
use std::str::FromStr;
use std::iter::FromIterator;

#[derive(Clone, Debug)]
enum Element {
    Number(usize),
    List(Vec<Element>)
}
use Element::*;

impl Element {
    fn singleton(x: Element) -> Element {
        List(vec![x])
    }
}

impl FromStr for Element {
    type Err = ();
    fn from_str(input: &str) -> Result<Self, ()>{
        if input.starts_with('[') {
            Ok(parse_list(&mut input[1..].chars()))
        } else {
            Err(())
        }
    }
}

impl PartialEq for Element {
    fn eq(&self, other: &Self) -> bool {
        return self.cmp(other) == Equal
    }
}
impl Eq for Element {}

impl PartialOrd<Element> for Element {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering>{
        Some(self.cmp(other))
    }
}

impl Ord for Element {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Number(x), Number(y)) => x.cmp(y),
            (Number(x), y) => Element::singleton(Number(*x)).cmp(y),
            (x, Number(y)) => x.cmp(&Element::singleton(Number(*y))),
            (List(x), List(y)) => {
                for (x, y) in x.iter().zip(y.iter()) {
                    match x.cmp(y) {
                        Equal => (),
                        v => return v
                    }
                }
                x.len().cmp(&y.len())
            }
        }
    }
}

fn parse_number(mut c: char, input: &mut impl Iterator<Item=char>) -> (Element, char) {
    let mut cs = Vec::new();
    while c.is_ascii_digit() {
        cs.push(c);
        c = input.next().unwrap();
    }
    return (Number(String::from_iter(cs).parse().unwrap()), c);
}

fn parse_list(input: &mut impl Iterator<Item=char>) -> Element {
    let mut l = Vec::new();
    let mut c = input.next().unwrap();
    let mut n;
    while c != ']' {
        if c == '[' {
            l.push(parse_list(input));
        } else if c.is_ascii_digit() {
            (n, c) = parse_number(c, input);
            l.push(n);
            continue
        }
        c = input.next().unwrap();
    }
    List(l)
}

fn main() {
    let mut packets: Vec<Element> = std::io::stdin().lines()
        .filter_map(Result::ok)
        .filter_map(|l| l.parse().ok())
        .collect();

    let mut sum: usize = 0;
    for i in 0..packets.len()/2 {
        if packets[i*2] < packets[i*2+1] {
            sum += i+1;
        }
    }
    println!("{}", sum);

    let divider_a: Element = "[[2]]".parse().unwrap();
    let divider_b: Element = "[[6]]".parse().unwrap();
    packets.push(divider_a.clone());
    packets.push(divider_b.clone());
    packets.sort();

    println!("{}",
             (packets.iter().position(|p| p == &divider_a).unwrap() + 1) *
             (packets.iter().position(|p| p == &divider_b).unwrap() + 1))
}

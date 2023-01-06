use std::collections::HashSet;
use std::fmt::Debug;
use std::iter::repeat;
use std::ops::{Add, AddAssign, Sub, SubAssign, Deref};

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
struct Coord (i32, i32);

//impl Coord {}
const ZERO: Coord = Coord(0, 0);
const LEFT: Coord = Coord(-1, 0);
const RIGHT: Coord = Coord(1, 0);
const UP: Coord = Coord(0, 1);
const DOWN: Coord = Coord(0, -1);

impl Add for Coord {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {self.add(&rhs)}
}
impl Add for &Coord {
    type Output = <Self as Deref>::Target;
    fn add(self, rhs: Self) -> Self::Output {
        (*self).add(rhs)  // NOTE: Implicit copy here due to Copy trait.
    }
}
impl Add<&Self> for Coord {
    type Output = Self;
    fn add(mut self, rhs: &Self) -> Self::Output {self += rhs; self}
}
impl AddAssign<&Self> for Coord {
    fn add_assign(&mut self, rhs: &Self) {
        self.0 += rhs.0;
        self.1 += rhs.1;
    }
}

impl Sub for Coord {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {self.sub(&rhs)}
}
impl Sub for &Coord {
    type Output = <Self as Deref>::Target;
    fn sub(self, rhs: Self) -> Self::Output {
        (*self).sub(rhs)  // NOTE: Implicit copy here due to Copy trait.
    }
}
impl Sub<&Self> for Coord {
    type Output = Self;
    fn sub(mut self, rhs: &Self) -> Self::Output {self -= rhs; self}
}
impl SubAssign<&Self> for Coord {
    fn sub_assign(&mut self, rhs: &Self) {
        self.0 -= rhs.0;
        self.1 -= rhs.1;
    }
}

fn tail_velocity(parent_displacement: Coord) -> Coord {
    let stretched = |x: i32| x.abs() > 1;
    if stretched(parent_displacement.0) || stretched(parent_displacement.1) {
        Coord (parent_displacement.0.signum(), parent_displacement.1.signum())
    } else {
        ZERO
    }
}

fn pull<'a>(h: &mut Coord, t: &'a mut Coord) -> &'a mut Coord {
    *t += &tail_velocity(*h - *t); t
}

fn parse_instruction(s: &str) -> Option<impl Iterator<Item=Coord>> {
    let (cmd, n) = s.trim().split_once(' ')?;
    Some(repeat(match cmd {
        "R" => RIGHT, "L" => LEFT, "U" => UP, "D" => DOWN,
        _ => panic!("panik!")
    }).take(n.parse().ok()?))
}

fn main() {
    let moves: Vec<_> = std::io::stdin().lines()
        .flat_map(|l| {
            l.ok().and_then(|l| parse_instruction(&l)).unwrap()
        }).collect();

    println!("{}", moves.iter().scan((ZERO, ZERO), |(h, t), v| {
        *h += &v; Some(*pull(h, t))
    }).collect::<HashSet<_>>().len());

    println!("{:?}", moves.iter().scan([ZERO; 10], |knots, v| {
        knots[0] += v;
        knots.iter_mut().reduce(pull).map(|x| *x)
    }).collect::<HashSet<_>>().len());
}

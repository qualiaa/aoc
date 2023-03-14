use std::ops::{Add,AddAssign,Deref};
use std::collections::HashSet;
use std::str::FromStr;
use std::iter::IntoIterator;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
struct Coord(isize, isize, isize);
type Occupancy = HashSet<Coord>;

impl Add<Self> for Coord {
    type Output = Self;
    fn add(self, rhs: Self) -> <Self as Add<Self>>::Output {
        self + &rhs
    }
}
impl Add<&Self> for Coord {
    type Output = Self;
    fn add(mut self, rhs: &Self) -> <Self as Add<&Self>>::Output {
        self += rhs;
        self
    }
}
impl Add<Self> for &Coord {
    type Output = <Self as Deref>::Target;
    fn add(self, rhs: Self) -> <Self as Add<Self>>::Output {
        self.clone() + rhs
    }
}
impl Add<Coord> for &Coord {
    type Output = <Self as Deref>::Target;
    fn add(self, rhs: Coord) -> <Self as Add<Coord>>::Output {
        self + &rhs
    }
}
impl AddAssign<Self> for Coord {
    fn add_assign(&mut self, rhs: Self) {
        *self += &rhs
    }
}
impl AddAssign<&Self> for Coord {
    fn add_assign(&mut self, rhs: &Self) {
        self.0 += rhs.0;
        self.1 += rhs.1;
        self.2 += rhs.2;
    }
}

impl FromStr for Coord {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, <Self as FromStr>::Err> {
        let parts: Result<Vec<isize>, _> = s.split(',').map(str::parse).collect();
        if let Ok(parts) = parts {
            return match parts[..] {
                [x, y, z] => Ok(Coord(x, y, z)),
                _ => Err(())
            }
        }
        return Err(())
    }
}

impl Coord {
    fn min(&self, other: &Coord) -> Coord {
        Coord(self.0.min(other.0),
              self.1.min(other.1),
              self.2.min(other.2))
    }
    fn max(&self, other: &Coord) -> Coord {
        Coord(self.0.max(other.0),
              self.1.max(other.1),
              self.2.max(other.2))
    }

    fn neighbours<'a>(&'a self) -> impl Iterator<Item=Coord> + 'a {
        Coord::DIRECTIONS.iter().map(move |d| self + d)
    }

    const MAX: Coord = Coord(isize::MAX, isize::MAX, isize::MAX);
    const MIN: Coord = Coord(isize::MIN, isize::MIN, isize::MIN);
    const DIRECTIONS: [Coord; 6] = [
        Coord(-1,  0,  0), Coord( 1,  0,  0),
        Coord( 0, -1,  0), Coord( 0,  1,  0),
        Coord( 0,  0, -1), Coord( 0,  0,  1)
    ];
}

fn plane(x0: isize, x1: isize, y0: isize, y1: isize) -> impl Iterator<Item=(isize, isize)> {
    (x0..=x1).flat_map(move |x| (y0..=y1).map(move |y| (x, y)))
}

fn make_frontier(shape: &Occupancy, min: Coord, max: Coord) -> Occupancy {
    let xy = plane(min.0, max.0, min.1, max.1).flat_map(|(x, y)| {
        IntoIterator::into_iter([Coord(x, y, min.2), Coord(x, y, max.2)])});
    let xz = plane(min.0, max.0, min.2, max.2).flat_map(|(x, z)| {
        IntoIterator::into_iter([Coord(x, min.1, z), Coord(x, max.1, z)])});
    let yz = plane(min.1, max.1, min.2, max.2).flat_map(|(y, z)| {
        IntoIterator::into_iter([Coord(min.0, y, z), Coord(max.0, y, z)])});
    xy.chain(xz).chain(yz).filter(|p| !shape.contains(p)).collect()
}

fn fill_exterior(mut shape: Occupancy) -> Occupancy {
    const SUCC: Coord = Coord(1, 1, 1);
    const PRED: Coord = Coord(-1, -1, -1);

    // Inefficient way of doing this but the efficient way is only tedious.

    fn go(marked: Occupancy, frontier: Occupancy) -> Occupancy {
        if frontier.is_empty() {
            marked
        } else {
            go(marked.union(&frontier).cloned().collect(),
               frontier.iter()
               .flat_map(|p| p.neighbours())
               .filter(|p| !marked.contains(&p) && !frontier.contains(&p))
               .collect())
        }
    }
    let (min, max) = shape.iter().fold(
        (Coord::MAX, Coord::MIN), |(min, max), p| (min.min(&p), max.max(&p)));
    let outer_shell = make_frontier(&shape, &min+PRED, &max+SUCC);
    let inner_shell = make_frontier(&shape, min, max);
    shape.extend(outer_shell.into_iter());
    go(shape, inner_shell)
}

fn main() {
    let droplet: Occupancy = std::io::stdin().lines()
        .flat_map(|x|x)
        .map(|l| l.parse().unwrap())
        .collect();

    let faces: Vec<_> = droplet.iter().flat_map(
        |p: &Coord| p.neighbours().filter(|q| !droplet.contains(q))).collect();

    println!("{}", faces.len());

    let exterior_points = fill_exterior(droplet);
    println!("{}", faces.into_iter().filter(|p| exterior_points.contains(p)).count());
}

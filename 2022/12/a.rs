use std::cmp::Reverse;
use std::convert::TryInto;
use std::collections::{BinaryHeap, HashMap};
use std::iter::IntoIterator;
use std::ops::{Add, Sub, Deref, Index};
use std::rc::Rc;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Point(isize, isize);

impl Point {
    fn l1(&self) -> usize {
        (self.0.abs() + self.1.abs()) as usize
    }
}

impl Add for &Point {
    type Output = <Self as Deref>::Target;
    fn add(self, other: Self) -> Self::Output {
        Point ( self.0 + other.0, self.1 + other.1 )
    }
}

impl Sub for &Point {
    type Output = <Self as Deref>::Target;
    fn sub(self, other: Self) -> Self::Output {
        Point ( self.0 - other.0, self.1 - other.1 )
    }
}

struct Map {
    map: Vec<Vec<u8>>,
    shape: Point
}

impl Map {
    fn new(map: Vec<Vec<u8>>) -> Self {
        let shape = Point(map[0].len() as isize, map.len() as isize);
        Map {map, shape}
    }

    fn fastest_route(&self, start: &Point, end: &Point) -> Option<usize> {
        let heuristic = |x: &Point| (end - x).l1();
        let mut distances: HashMap<Rc<Point>, usize> = HashMap::new();
        let mut pq = BinaryHeap::new();
        let start = Rc::new(start.clone());
        distances.insert(Rc::clone(&start), 0);
        pq.push((Reverse(heuristic(&start)), start));

        while let Some((_, p)) = pq.pop() {
            let distance = distances[&p] + 1;
            for q in self.accessible_neighbours(&p) {
                let q = Rc::new(q);
                let prior_distance = distances.get(&q).copied();
                if distance < prior_distance.unwrap_or(usize::MAX) {
                    pq.push((Reverse(heuristic(&q) + distance), Rc::clone(&q)));
                    distances.insert(Rc::clone(&q), distance);
                }
                if *q == *end {
                    return Some(distances.get(&q).copied().unwrap());
                }
            }
        }
        None
    }

    fn in_bounds(&self, p: &Point) -> bool {
        p.0 >= 0 && p.1 >= 0 && p.0 < self.shape.0 && p.1 < self.shape.1
    }

    fn accessible_neighbours(&self, p: &Point) -> Vec<Point> {
        IntoIterator::into_iter([p + &Point(1, 0),
                                 p + &Point(-1, 0),
                                 p + &Point(0, 1),
                                 p + &Point(0, -1)])
            .filter(|q: &Point| self.in_bounds(q) && self[q] <= self[p]+1)
            .collect()
    }
}

impl Index<&Point> for Map {
    type Output = u8;
    fn index(&self, index: &Point) -> &Self::Output {
        &self.map[index.1 as usize][index.0 as usize]
    }
}


fn main() {
    let mut start = Point(0, 0);
    let mut end = Point(0, 0);
    let map = Map::new(
        std::io::stdin().lines()
            .enumerate()
            .map(|(y, l)| l.unwrap().trim().bytes().enumerate().map(
                |(x, h)| match h.into() {
                    'S' => {start = Point(x as isize, y as isize); 'a'.try_into().unwrap()},
                    'E' => {end = Point(x as isize, y as isize); 'z'.try_into().unwrap()},
                    _ => h
                }).collect()
            ).collect());
    println!("{}", map.fastest_route(&start, &end).unwrap())

}

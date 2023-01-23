use std::cmp::Reverse;
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

    fn neighbours(&self) -> impl Iterator<Item=Point> {
        IntoIterator::into_iter([self + &Point(1, 0),
                                 self + &Point(-1, 0),
                                 self + &Point(0, 1),
                                 self + &Point(0, -1)])
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

type Distances = HashMap<Rc<Point>, usize>;

impl Map {
    fn new(map: Vec<Vec<u8>>) -> Self {
        let shape = Point(map[0].len() as isize, map.len() as isize);
        Map {map, shape}
    }

    fn fastest_route_once(
        &self,
        start: Point,
        end: &Point,
        valid_neighbour: impl FnMut(&Map, &Point, &Point) -> bool
    ) -> Option<usize> {
        self.fastest_route(start, end, valid_neighbour, &mut HashMap::new())
    }

    fn fastest_route(
        &self,
        start: Point,
        end: &Point,
        mut valid_neighbour: impl FnMut(&Map, &Point, &Point) -> bool,
        distances: &mut Distances
    ) -> Option<usize> {
        let heuristic = |x: &Point| (end - x).l1();

        let mut pq = BinaryHeap::new();

        let start = Rc::new(start);
        distances.insert(Rc::clone(&start), 0);
        pq.push((Reverse(heuristic(&start)), start));

        while let Some((_, p)) = pq.pop() {
            let distance = distances[&p] + 1;
            let accessible_neighbours = p.neighbours()
                .filter(|q: &Point| self.in_bounds(q) && valid_neighbour(self, &p, q))
                .map(Rc::new);
            for q in accessible_neighbours {
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

    fn points(&self) -> impl Iterator<Item=Point> + '_ {
        (0..self.shape.1).flat_map(move |y| (0..self.shape.0).map(move |x| Point (x, y)))
    }
}

fn gentle_incline(map: &Map, p: &Point, q: &Point) -> bool {
    map[q] <= map[&p]+1
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
                |(x, h)| match h {
                    b'S' => {start = Point(x as isize, y as isize); b'a'},
                    b'E' => {end = Point(x as isize, y as isize); b'z'},
                    h => h
                }).collect()
            ).collect());

    println!("{}", map.fastest_route_once(start, &end, gentle_incline).unwrap());
    println!("{}",
             map.points().filter(
                 |p| map[p] == b'a' && p.neighbours().any(
                     // We take 'a' values which border a 'b' because...
                     |q| map.in_bounds(&q) && map[&q] == b'b'))
             .scan(HashMap::new(),
                   |mut distances, start| Some(map.fastest_route(
                       start, &end,
                       // ... there is no point visiting an 'a',
                       // as we can start at any 'a'
                       |m,p,q| m[q] != b'a' && gentle_incline(m, p, q),
                       &mut distances)))
             .filter_map(std::convert::identity)
             .min().unwrap())
}

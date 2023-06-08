use std::collections::{BinaryHeap, HashMap, HashSet};
use std::cmp::Reverse;
use std::iter::IntoIterator;
use std::hash::Hash;
use std::rc::Rc;

fn fastest_route<P, CompletionFn, NeighbourFn, Neighbours, HeuristicFn>(
    start: P, complete: CompletionFn, neighbours: NeighbourFn, heuristic: HeuristicFn)
    -> Option<usize>
    where P: Eq + Hash + Ord,
          CompletionFn: Fn(&P) -> bool,
          HeuristicFn: Fn(&P) -> usize,
          NeighbourFn: Fn(&P) -> Neighbours,
          Neighbours: Iterator<Item=P> {
    let start = Rc::new(start);
    let mut distances: HashMap<Rc<P>, usize> = HashMap::new();
    distances.insert(Rc::clone(&start), 0);

    let mut pq = BinaryHeap::new();
    pq.push((Reverse(heuristic(&start)), start));

    while let Some((_, p)) = pq.pop() {
        let distance = distances[&p] + 1;

        for q in neighbours(&p).map(Rc::new) {
            let prior_distance = distances.get(&q).copied();
            if distance < prior_distance.unwrap_or(usize::MAX) {
                pq.push((Reverse(heuristic(&q) + distance), Rc::clone(&q)));
                distances.insert(Rc::clone(&q), distance);
            }
            if complete(&q) {
                return Some(distances.get(&q).copied().unwrap());
            }
        }
    }
    None
}

fn modulo(x: isize, y: isize) -> isize {
    let z = x % y;
    if z < 0 {y + z} else {z}
}

fn l1_distance(a: (isize, isize), b: (isize, isize)) -> usize {
    ((a.0 - b.0).abs() + (a.1 - b.1).abs()) as usize
}

#[derive(PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Point {
    x: isize,
    y: isize,
    t: isize
}

impl Point {
    fn from_pair(p: (isize, isize), t: isize) -> Self {
        Point {x: p.0, y: p.1, t}
    }

    fn neighbours(&self) -> impl Iterator<Item=Point> {
        let (x, y, t) = (self.x, self.y, self.t+1);
        IntoIterator::into_iter([
            Point{x, y, t},
            Point{x, y: y+1, t},
            Point{x, y: y-1, t},
            Point{x: x+1, y, t},
            Point{x: x-1, y, t}
        ])
    }
}

type Blizzard = HashSet<(isize, isize)>;
struct Map {
    left: Blizzard,
    right: Blizzard,
    up: Blizzard,
    down: Blizzard,
    width: isize,
    height: isize
}

impl Map {
    fn in_bounds(&self, p: &Point) -> bool {
        (p.x, p.y) == self.start() || (p.x, p.y) == self.end() ||
        p.x > 0 && p.y > 0 && p.x + 1 < self.width && p.y + 1 < self.height
    }

    fn start(&self) -> (isize, isize) {
        (1, 0)
    }

    fn end(&self) -> (isize, isize) {
        (self.width-2, self.height-1)
    }

    fn from_lines(lines: &[String]) -> Self {
        let (height, width) = (lines.len() as isize, lines[0].len() as isize);

        type B = Blizzard;

        let [left, right, up, down] = lines.into_iter()
            .enumerate()
            .fold([B::new(), B::new(), B::new(), B::new()],
                  |bs, (y, l)|
                  l.chars().enumerate().fold(bs, |mut bs, (x, c)| {
                      let point = (x as isize, y as isize);
                      match c {
                          '<' => bs[0].insert(point),
                          '>' => bs[1].insert(point),
                          '^' => bs[2].insert(point),
                          'v' => bs[3].insert(point),
                          _ => Default::default()
                      };
                      bs
                  }));

        Map { left, right, up, down, width, height }
    }

    fn blocked(&self, p: &Point) -> bool {
        if (p.x, p.y) == self.start() || (p.x, p.y) == self.end() {
            return false
        }
        let up_y = 1 + modulo(p.y - 1 + p.t, self.height - 2);
        let down_y = 1 + modulo(p.y - 1 - p.t, self.height - 2);
        let left_x = 1 + modulo(p.x - 1 + p.t, self.width - 2);
        let right_x = 1 + modulo(p.x - 1 - p.t, self.width - 2);
        self.up.contains(&(p.x, up_y)) || self.down.contains(&(p.x, down_y))
            || self.left.contains(&(left_x, p.y)) || self.right.contains(&(right_x, p.y))
    }

    fn allowed(&self, p: &Point) -> bool {
        self.in_bounds(p) && !self.blocked(p)
    }

    fn fastest_route(&self, start: Point, target: (isize, isize)) -> Option<usize> {
        fastest_route(
            start,
            |p| (p.x, p.y) == target,
            |p| p.neighbours().filter(|q| self.allowed(q)),
            |p| l1_distance((p.x, p.y), target)
        )
    }
}


fn main() {
    let map = Map::from_lines(
        &std::io::stdin()
            .lines()
            .flat_map(|l| l).collect::<Vec<String>>());

    let a = map.fastest_route(Point::from_pair(map.start(), 0), map.end()).unwrap();
    println!("{}", a);

    let b = map.fastest_route(Point::from_pair(map.end(), a as isize), map.start()).unwrap();
    let c = map.fastest_route(Point::from_pair(map.start(), (a+b) as isize), map.end()).unwrap();
    println!("{}", a+b+c);
}

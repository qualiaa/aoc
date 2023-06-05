use std::collections::{VecDeque, HashMap, HashSet};
use std::iter::IntoIterator;

type Coord = (isize, isize);
type Elves = HashSet<Coord>;

#[derive(Clone, Copy)]
enum Direction {North, South, West, East}
use Direction::*;

impl Direction {
    fn fov(self, &(x, y): &Coord) -> impl Iterator<Item=Coord> {
        IntoIterator::into_iter(match self {
            North => [(x, y-1), (x-1, y-1), (x+1, y-1)],
            South => [(x, y+1), (x-1, y+1), (x+1, y+1)],
            West => [(x-1, y), (x-1, y-1), (x-1, y+1)],
            East => [(x+1, y), (x+1, y-1), (x+1, y+1)]
        })
    }

    fn next(self, &(x, y): &Coord) -> Coord {
        match self {
            North => (x, y-1),
            South => (x, y+1),
            West => (x-1, y),
            East => (x+1, y),
        }
    }

    const DIRECTIONS: [Direction; 4] = [North, South, West, East];
}

fn step(mut elves: Elves, dirs: &VecDeque<Direction>) -> (Elves, bool) {
    // to -> from
    let mut proposals: HashMap<Coord, Coord> = HashMap::new();

    for from in elves.iter() {
        let scan: Vec<_> = dirs.iter()
            .filter(|d| !d.fov(from).any(|c| elves.contains(&c)))
            .map(|d| d.next(from)).collect();
        match scan[..] {
            [] => continue,
            [_, _, _, _] => continue,
            [to, ..] => {
                if proposals.remove(&to).is_none() {
                    proposals.insert(to, from.clone());
                }
            }
        }
    }

    let finished = proposals.is_empty();
    for (to, from) in proposals {
        elves.remove(&from);
        elves.insert(to);
    }
    (elves, finished)
}

fn aabb(elves: &Elves) -> ((isize, isize), (isize, isize)) {
    elves.iter()
        .fold(((isize::MAX, isize::MAX), (isize::MIN, isize::MIN)),
              |((xmin, ymin), (xmax, ymax)), (x, y)| ((xmin.min(*x), ymin.min(*y)),
                                                      (xmax.max(*x), ymax.max(*y))))
}

fn main() {
    let mut elves: Elves = std::io::stdin().lines()
        .flat_map(|l| l)
        .enumerate()
        .flat_map(|(y, l)| l.chars().enumerate()
             .filter_map(|(x, c)| if c == '#' {Some((x as isize, y as isize))} else {None})
                  .collect::<Vec<_>>())
        .collect();
    let num_elves = elves.len() as isize;

    let mut dirs: VecDeque<_> = Direction::DIRECTIONS.iter().cloned().collect();

    for _ in 0..10 {
        (elves, _) = step(elves, &dirs);
        dirs.rotate_left(1);
    }

    let ((xmin, ymin), (xmax, ymax)) = aabb(&elves);
    println!("{}", (xmax - xmin + 1)*(ymax - ymin + 1) - num_elves);

    for i in 10.. {
        let finished;
        (elves, finished) = step(elves, &dirs);
        if finished {
            println!("{}", i+1);
            break
        }
        dirs.rotate_left(1);
    }
}

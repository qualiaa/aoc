use std::collections::HashSet;
use std::ops::{Add, AddAssign, Deref};
use std::str::FromStr;
use std::iter::IntoIterator;
use std::io::Read;
use std::cmp::max;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Coord(isize, isize);

impl Add<Self> for Coord {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        self + &rhs
    }
}
impl AddAssign<Self> for Coord {
    fn add_assign(&mut self, rhs: Self) {
        *self += &rhs
    }
}
impl Add<&Self> for Coord {
    type Output = Self;
    fn add(mut self, rhs: &Self) -> Self::Output {
        self += rhs;
        self
    }
}
impl AddAssign<&Self> for Coord {
    fn add_assign(&mut self, rhs: &Self) {
        self.0 += rhs.0;
        self.1 += rhs.1;
    }
}
impl Add<Self> for &Coord {
    type Output = <Self as Deref>::Target;
    fn add(self, rhs: Self) -> Self::Output {
        self.clone() + rhs
    }
}
impl Add<Coord> for &Coord {
    type Output = <Self as Deref>::Target;
    fn add(self, rhs: Coord) -> Self::Output {
        rhs + self
    }
}

const LEFT: Coord = Coord(-1, 0);
const RIGHT: Coord = Coord(1, 0);
const DOWN: Coord = Coord(0, -1);

type Shape = HashSet<Coord>;
type Chamber = HashSet<Coord>;
#[derive(Debug, Clone)]
struct Tetromino {
    shape: Shape,
    width: usize,
    height: usize
}

impl FromStr for Tetromino {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, <Self as FromStr>::Err> {
        let lines: Vec<_> = s.trim().split('\n').collect();
        let height = lines.len();
        let shape: Shape = lines.into_iter().rev().enumerate().flat_map(|(j, line)|
            line.chars()
                .enumerate()
                .filter_map(move |(i, c)|
                    if c == '@' {Some(Coord(i as isize, j as isize))} else {None})
        ).collect();
        let width = shape.iter().map(|Coord(i, _)| *i).max().unwrap() as usize;
        Ok(Tetromino { shape, width, height })
    }
}

impl Tetromino {
    fn collision(&self, position: &Coord, chamber: &Chamber) -> bool {
        self.shape.iter().map(|p| p+position).any(|p| chamber.contains(&p))
    }

    fn out_of_bounds(&self, position: &Coord) -> bool {
        position.1 < 0 || position.0 < 0 || position.0 + self.width as isize > 6
    }

    fn check(&self, position: Coord, chamber: &Chamber) -> Option<Coord> {
        if self.out_of_bounds(&position) || self.collision(&position, &chamber) {
            None
        } else {
            Some(position)
        }
    }

    fn step(&self, position: Coord, shift: Coord, chamber: &Chamber) -> Result<Coord, Coord> {
        // Very normal thing to write
        self.check(&position + shift, &chamber).ok_or(()).or(Ok(position)).and_then(|p| {
            self.check(&p + DOWN, &chamber).ok_or(p)
        })
    }

    fn place(&self, position: &Coord, chamber: &mut Chamber) {
        for p in self.shape.iter() {
            chamber.insert(p+position);
        }
    }

    fn settle(&self, mut position: Coord, mut chamber: &mut Chamber, moves: &mut impl Iterator<Item=Coord>) -> Coord {
        for shift in moves {
            match self.step(position, shift, &chamber) {
                Ok(new_position) => position = new_position,
                Err(last_position) => {
                    self.place(&last_position, &mut chamber);
                    return last_position
                }
            }
        }
        panic!("unreachable");
    }

    fn cycle() -> impl Iterator<Item=Tetromino> {
        IntoIterator::into_iter([
            r#"
@@@@
"#,

            r#"
.@.
@@@
.@.
"#,
            r#"
..@
..@
@@@
"#,
            r#"
@
@
@
@"#,
            r#"
@@
@@"#
        ]).map(Tetromino::from_str).filter_map(Result::ok).cycle()
    }
}

fn find_cycle<T: Eq>(xs: &[T], min: usize) -> Option<(usize, usize)> {
    for cycle_size in (min..=xs.len()/2).rev() {
        let (start, end) = xs.split_at(xs.len()-cycle_size);
        if start.ends_with(end) {
            return Some((cycle_size, xs.len() - 2 * cycle_size))
        }
    }
    return None;
}

fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();
    let instructions: Vec<_> = input.trim().chars().map(|c| match c {
        '<' => LEFT,
        '>' => RIGHT,
        _ => panic!("panik!")
    }).collect();
    let num_instructions = instructions.len();

    let target_pieces: usize = 1_000_000_000_000;
    let mut chamber = HashSet::new();
    let mut instruction_cycle = instructions.iter().cloned().cycle();
    let iter = Tetromino::cycle()
               .scan((0, 0), |(last_y, steps), piece| {
                   let start_y = *last_y as isize + 3;
                   let end_y = piece.settle(Coord(2, start_y), &mut chamber, &mut instruction_cycle).1;
                   let max_y = max(end_y as usize + piece.height, *last_y);
                   *last_y = max_y;
                   *steps += start_y - end_y;
                   Some((max_y as usize, *steps as usize))
               });

    let mut all_heights = Vec::<usize>::new();
    let mut heights = Vec::<usize>::new();
    let mut deltas = Vec::<usize>::new();
    let mut pieces = Vec::<usize>::new();

    for (piece, (tower_height, step)) in iter.enumerate().map(|(i,x)| (i+1, x)) {
        if piece == 2022 {
            println!("{}", tower_height);
        }

        all_heights.push(tower_height);

        if step % num_instructions != 0 || piece % 5 != 0 {continue}

        deltas.push(tower_height - heights.last().unwrap_or(&0));
        heights.push(tower_height);
        pieces.push(piece);

        if let Some((cycle_size, start_index)) = find_cycle(&deltas, 349) {
            let start_pieces = pieces[start_index];
            let end_pieces = pieces[start_index + cycle_size];
            let cycle_pieces = end_pieces - start_pieces;
            let start_height = heights[start_index];
            let end_height = heights[start_index + cycle_size];
            let cycle_height = end_height - start_height;
            let skipped_cycles = (target_pieces - start_pieces) / cycle_pieces;

            let remaining_pieces = target_pieces - start_pieces - skipped_cycles * cycle_pieces;
            let skipped_height = skipped_cycles * cycle_height;

            println!("{}", start_height + skipped_height + (all_heights[start_pieces + remaining_pieces - 1] - all_heights[start_pieces-1]));
            break;
        }
    }
}

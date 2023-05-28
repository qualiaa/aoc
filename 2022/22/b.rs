#![feature(generic_const_exprs)]
#![feature(maybe_uninit_array_assume_init)]

use std::collections::{HashMap, HashSet};
use std::ops::{Add, AddAssign, Index, IndexMut, Mul, Neg, Sub, Deref};
use std::mem::{MaybeUninit};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Coord(isize, isize);
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
    }
}

enum Tile {
    Wall,
    Empty
}

struct Map {
    map: HashMap<Coord, Tile>,
    max: Coord,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum Direction {East, South, West, North}
use Direction::*;

#[derive(Copy, Clone)]
enum Rotation {CCW = -1, CW = 1}

enum Instruction {
    Turn(Rotation),
    Move(usize)
}

impl Coord {
    fn max(&self, other: &Coord) -> Coord {
        Coord(self.0.max(other.0),
              self.1.max(other.1))
    }

    const MIN: Coord = Coord(isize::MIN, isize::MIN);

    fn from_direction(d: &Direction) -> Coord {
        match d {
            North => Coord(0, -1),
            East => Coord(1, 0),
            South => Coord(0, 1),
            West => Coord(-1, 0),
        }
    }

    fn neighbours<'a>(&'a self) -> impl Iterator<Item=Coord> + 'a {
        Direction::DIRECTIONS.iter().map(Coord::from_direction).map(move |d| self + d)
    }
}

fn modulo(x: isize, y: isize) -> isize {
    let z = x % y;
    if z < 0 {y + z} else {z}
}

impl Direction {
    fn rotate(&self, rot: Rotation) -> Direction {
        let rot = modulo(rot as isize, *self as isize) as usize;
        Direction::DIRECTIONS[Direction::DIRECTIONS.len() - rot]
    }

    const DIRECTIONS: [Direction; 4] = [East, North, West, South]; // order matters
}

impl Instruction {
    fn execute(self, pose: Pose, map: &Map) -> Pose {
        use Instruction::*;
        let (mut position, direction) = pose;
        match self {
            Turn(rotation) => (position, direction.rotate(rotation)),

            Move(n) => {
                for _ in 0..n {
                    if let Some(new_position) = map.next(position.clone(), direction) {
                        position = new_position;
                    } else {
                        break;
                    }
                }
                (position, direction)
            }
        }
    }
}

type Pose = (Coord, Direction);
type CharGrid = Vec<Vec<char>>;


impl Map {
    fn new(map: HashMap<Coord, Tile>) -> Self {
        let max = map.keys().fold(Coord::MIN, |c0, c1| c0.max(c1));
        Map { map, max }
    }

    fn from_lines(lines: impl Iterator<Item=String>) -> Self {
        use Tile::*;

        Map::new(lines.enumerate().flat_map(|(y, l)| {
            l.chars().enumerate().filter_map(move |(x, c)| {
                match c {
                    '#' => Some(Wall),
                    '.' => Some(Empty),
                    _ => None
                }.map(|t| (Coord(x as isize, y as isize), t))
            }).collect::<Vec<(Coord, Tile)>>()
        }).collect())
    }

    fn initial_pose(&self) -> Pose {
        let start_point: &Coord = self.map.keys().filter(|Coord(_, y)| *y == 0).min_by_key(|Coord(x, _)| x).unwrap();
        (start_point.clone(), East)
    }

    fn next(&self, mut position: Coord, direction: Direction) -> Option<Coord> {
        use Tile::*;
        position += Coord::from_direction(&direction);
        if !self.map.contains_key(&position) {
            position = self.wrap(position, direction);
        }
        match self.map[&position] {
            Empty => Some(position),
            Wall => None
        }
    }

    fn wrap(&self, point: Coord, direction: Direction) -> Coord {
        let mut point = match direction {
            North => Coord(point.0, self.max.1),
            East => Coord(0, point.1),
            South => Coord(point.0, 0),
            West => Coord(self.max.0, point.1)
        };
        while !self.map.contains_key(&point) {
            point += Coord::from_direction(&direction);
        }
        point
    }
}


fn parse_instructions(mut s: &str) -> Vec<Instruction> {
    use Instruction::*;
    use Rotation::*;
    let mut instructions = Vec::new();

    while !s.is_empty() {
        instructions.push(match s.chars().next() {
            Some('R') => { s = &s[1..]; Turn(CW) },
            Some('L') => { s = &s[1..]; Turn(CCW) },
            _ => {
                let pos = s.chars().position(|c| c=='R' || c=='L').unwrap_or(s.len());
                let val = Move(s[..pos].parse().unwrap());
                s = &s[pos..];
                val
            }
        })
    }
    instructions
}

fn password(pose: Pose) -> usize {
    let (pos, facing) = pose;
    (1000 * (pos.1 + 1) + 4 * (pos.0 + 1) + facing as isize) as usize
}

fn lines_to_char_grid(lines: impl IntoIterator<Item=String>) -> CharGrid {
    lines.into_iter().map(|l| l.chars().collect()).collect()
}

fn find_faces(map: CharGrid, tile_size: usize) -> HashSet<Coord> {
    let (width, height) = (map.iter().map(Vec::len).max().unwrap(), map.len());

    let mut h = HashSet::new();
    for y in 0..height / tile_size {
        for x in 0..width / tile_size {
            match map[y * tile_size].get(x * tile_size) {
                Some(&c) if c != ' ' => h.insert(Coord(x as isize, y as isize)),
                _ => continue,
            };
        }
    }
    h
}

struct Mat<T, const M: usize, const N: usize> ([T; M * N]) where [T; M * N]: Sized;

impl<T, const M: usize, const N: usize> Mat<T, M, N>
where [T; M*N]: Sized {
    fn new(data: [T; M*N]) -> Self {
        Mat(data)
    }

    fn empty() -> Mat<MaybeUninit<T>, M, N> {
        Mat(unsafe {
            MaybeUninit::uninit().assume_init()
        })
    }
}

impl<T, const M: usize, const N: usize> Mat<T, M, N>
where [T; M*N]: Sized, [T; N*M]: Sized, T: Copy {
    fn transpose(&self) -> Mat<T, N, M> {
        let mut mat = Mat::<T, N, M>::empty();

        for i in 0..M {
            for j in 0..N {
                mat.write((i, j), self[(j, i)]);
            }
        }
        unsafe {mat.assume_init()}
    }
}

impl<T, const M: usize, const N: usize> Default for Mat<T, M, N>
where T: Default + Copy, // Copy shouldn't be necessary here but looks like Default for [x;N] only works for N<32
          [T; M*N]: Sized {
    fn default() -> Self {
        Mat([Default::default(); M*N])
    }
}

impl<const M: usize> Mat<isize, M, M>
where [isize; M*M]: Sized {
    fn identity() -> Self {
        let mut mat = Mat::<isize, M, M>::default();
        for i in 0..M {
            mat[(i, i)] = 1;
        }
        mat
    }
}

impl<T, const M: usize, const N: usize, const Q: usize> Mul<Mat<T, N, Q>> for &Mat<T, M, N>
where T: AddAssign + Default,
      for<'a> &'a T: Mul<Output=T>,
      [T; M*N]: Sized,
      [T; N*Q]: Sized {
    type Output = Mat<T, N, Q>;
    fn mul(self, rhs: Mat<T, N, Q>) -> Self::Output {
        let mut mat = Mat::<T, N, Q>::empty();
        for m in 0..M {
            for q in 0..Q {
                let mut sum = Default::default();
                for n in 0..N {
                    sum += &self[(m, n)] * &rhs[(n, q)];
                }
                mat.write((m, q), sum);
            }
        }
        unsafe {mat.assume_init()}
    }
}

impl<T> Mat<T, 3, 3>
where T: Default + Add + Mul + Neg<Output=T> + Copy {
    fn cross(x: (T, T, T)) -> Self {
        let z: T = Default::default();
        Mat([z, -x.2, x.1,
             x.2, z, -x.0,
             -x.1, x.0, z])
    }
}

impl<T, const M: usize, const N: usize> Mat<MaybeUninit<T>, M, N>
where [MaybeUninit<T>; M * N]: Sized {
    unsafe fn assume_init(self) -> Mat<T, M, N> {
        Mat(unsafe {
            // Following line bumps into: https://github.com/rust-lang/rust/issues/61956
            //mem::transmute::<_, [T; M*N]>(self.0)
            // This might be the preferred solution anyway:
            MaybeUninit::array_assume_init(self.0)
        })
    }

    fn write(&mut self, index: (usize, usize), t: T) {
        self[index].write(t);
    }
}

impl<T: Copy> Mat<T, 2, 1> {
    fn from_tuple(data: (T, T)) -> Self {
        Mat([data.0, data.1])
    }

    fn to_tuple(self) -> (T, T) {
        (self.0[0], self.0[1])
    }
}

impl<T: Copy> Mat<T, 3, 1> {
    fn from_tuple(data: (T, T, T)) -> Self {
        Mat([data.0, data.1, data.2])
    }

    fn to_tuple(self) -> (T, T, T) {
        (self.0[0], self.0[1], self.0[2])
    }
}

impl<T, const M: usize, const N: usize> Index<(usize, usize)> for Mat<T, M, N>
where [T; M * N]: Sized {
    type Output = T;
    fn index(&self, index: (usize, usize)) -> &Self::Output {
        &self.0[index.0 * N + index.1]
    }
}

impl<T, const M: usize, const N: usize> IndexMut<(usize, usize)> for Mat<T, M, N>
where [T; M * N]: Sized {
    fn index_mut(&mut self, index: (usize, usize)) -> &mut Self::Output {
        &mut self.0[index.0 * N + index.1]
    }
}

fn cross<T>(a: (T, T, T), b: (T, T, T)) -> (T, T, T)
where T: Mul<Output=T> + Sub<Output=T> + Copy {
    (a.1 * b.2 - a.2 * b.1,
     a.2 * b.0 - a.0 * b.2,
     a.0 * b.1 - a.1 * b.0)
}

fn cubify(face_coords: HashSet<Coord>) -> HashMap<(Coord, Direction), (Coord, Direction)> {
    // First, fold face_coords into 3D coordinates
    // (we take the first coord we get out of the set to be (0, 0, 0))
    type Coord3D = (isize, isize, isize);
    let mut faces: HashMap<Coord, Coord3D>= HashMap::new();
    let mut edges: HashMap<(Coord, Direction), Coord3D> = HashMap::new();

    let face_coord = face_coords.iter().next().unwrap();
}

fn main() {
    let mut lines: Vec<String> = std::io::stdin().lines().flat_map(|l| l).collect();
    let instructions = parse_instructions(&lines.pop().unwrap());
    let char_grid = lines_to_char_grid(lines);
    let face_coords = find_faces(char_grid, 4);
    println!("{:?}", face_coords);
    //let map = Map::from_lines(lines.into_iter());

    //let final_pose = instructions.into_iter().fold(map.initial_pose(), |p, i| i.execute(p, &map));
    //println!("{}", password(final_pose));
}

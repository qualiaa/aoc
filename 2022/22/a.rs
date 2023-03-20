use std::collections::{VecDeque, HashMap};
use std::ops::{Add, AddAssign, Deref};

#[derive(Clone, PartialEq, Eq, Hash)]
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
}

impl Direction{
    fn rotate(&self, rot: Rotation) -> Direction {
        let mut rotation = (rot as isize + *self as isize) % 4;
        if rotation < 0 {
            rotation = 4 + rotation;
        }
        let mut v = VecDeque::from([East, North, West, South]);
        v.rotate_right(rotation as usize);
        v[0]
    }
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

fn main() {
    let mut lines: Vec<String> = std::io::stdin().lines().flat_map(|l| l).collect();
    let instructions = parse_instructions(&lines.pop().unwrap());
    let map = Map::from_lines(lines.into_iter());

    let mut pose = map.initial_pose();
    for instruction in instructions {
        pose = instruction.execute(pose, &map);
    }
    println!("{}", password(pose));
}

#![feature(let_chains)]
#![feature(generic_const_exprs)]
#![feature(maybe_uninit_array_assume_init)]

use std::collections::{BTreeSet, HashMap, HashSet};
use std::ops::{Add, AddAssign, SubAssign, Mul, MulAssign, Deref};
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
impl SubAssign<Self> for Coord {
    fn sub_assign(&mut self, rhs: Self) {
        *self -= &rhs
    }
}
impl SubAssign<&Self> for Coord {
    fn sub_assign(&mut self, rhs: &Self) {
        self.0 -= rhs.0;
        self.1 -= rhs.1;
    }
}

impl Mul<isize> for Coord {
    type Output = Coord;
    fn mul(mut self, s: isize) -> Self::Output {
        self *= s;
        self
    }
}

impl Mul<isize> for &Coord {
    type Output = Coord;
    fn mul(self, s: isize) -> Self::Output {
        let mut new = self.clone();
        new *= s;
        new
    }
}

impl MulAssign<isize> for Coord {
    fn mul_assign(&mut self, s: isize) {
        self.0 *= s;
        self.1 *= s;
    }
}

enum Tile {
    Wall,
    Empty
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum Direction {East, South, West, North}
use Direction::*;

#[derive(Copy, Clone)]
enum Rotation {CCW = -1, CW = 1}

#[derive(Clone)]
enum Instruction {
    Turn(Rotation),
    Move(usize)
}

impl Coord {
    const MIN: Coord = Coord(isize::MIN, isize::MIN);

    fn from_direction(d: &Direction) -> Coord {
        match d {
            North => Coord(0, -1),
            East => Coord(1, 0),
            South => Coord(0, 1),
            West => Coord(-1, 0),
        }
    }

    fn elementwise_max(&self, other: &Coord) -> Coord {
        Coord(self.0.max(other.0), self.1.max(other.1))
    }

    fn to_tuple(&self) -> (isize, isize) {
        (self.0, self.1)
    }
}

fn modulo(x: isize, y: isize) -> isize {
    let z = x % y;
    if z < 0 {y + z} else {z}
}

impl Direction {
    fn rotate(self, rot: Rotation) -> Direction {
        Direction::DIRECTIONS[modulo(rot as isize + self as isize, 4) as usize]
    }

    fn opposite(self) -> Direction {
        Direction::DIRECTIONS[modulo(self as isize + 2, 4) as usize]
    }

    fn cw_rotations_to(self, other: Direction) -> usize {
        modulo(other as isize - self as isize, 4) as usize
    }

    const DIRECTIONS: [Direction; 4] = [East, South, West, North]; // order matters
}

impl Instruction {
    fn execute(self, mut pose: Pose, map: &Map) -> Pose {
        use Instruction::*;
        match self {
            Turn(rotation) => (pose.0, pose.1.rotate(rotation)),

            Move(n) => {
                for _ in 0..n {
                    if let Some(new_pose) = map.next(pose.clone()) {
                        pose = new_pose;
                    } else {
                        break;
                    }
                }
                pose
            }
        }
    }

    fn execute_all(instructions: impl Iterator<Item=Instruction>, map: &Map) -> Pose {
        instructions.fold(map.initial_pose(), |p, i| i.execute(p, &map))
    }
}

type Pose = (Coord, Direction);
type CharGrid = Vec<Vec<char>>;
type Face = HashMap<Coord, Tile>;
type Edges = HashMap<(Coord, Direction), (Coord, Direction)>;

struct Map {
    faces: HashMap<Coord, Face>,
    edges: Edges,
    face_size: usize
}

impl Map {
    fn new(faces: HashMap<Coord, Face>, edges: Edges, face_size: usize) -> Self {
        Map { faces, edges, face_size }
    }

    fn get(&self, face_coord: &Coord, local_coord: &Coord) -> &Tile {
        self.faces[face_coord].get(local_coord).unwrap()
    }

    fn initial_pose(&self) -> Pose {
        let start_point: &Coord = self.faces.keys()
            .filter(|Coord(_, y)| *y == 0)
            .min_by_key(|Coord(x, _)| x).unwrap();
        // FIXME: This doesn't work if there is a wall in the top left
        (start_point * self.face_size as isize, East)
    }

    fn next(&self, pose: Pose) -> Option<Pose> {
        use Tile::*;

        let (mut face_coord, mut pose) = self.to_face(pose);
        let face = self.faces.get(&face_coord).unwrap();

        pose.0 += Coord::from_direction(&pose.1);
        if !face.contains_key(&pose.0) {
            (face_coord, pose) = self.next_face(face_coord, pose);
        }
        match self.get(&face_coord, &pose.0) {
            Empty => Some(self.from_face(face_coord, pose)),
            Wall => None
        }
    }

    fn next_face(&self, face_coord: Coord, pose: Pose) -> (Coord, Pose) {
        let (mut local_coord, old_dir) = pose;
        let (new_face_coord, new_dir) = &self.edges[&(face_coord, old_dir)];

        let new_dir = new_dir.opposite();

        local_coord = self.wrap(local_coord);
        local_coord = match old_dir.cw_rotations_to(new_dir) {
            0 => local_coord,
            1 => Coord(self.face_size as isize - 1 - local_coord.1, local_coord.0),
            3 => Coord(local_coord.1, self.face_size as isize - 1 - local_coord.0),
            2 => Coord(self.face_size as isize - 1 - local_coord.0,
                       self.face_size as isize - 1 - local_coord.1),
            _ => panic!("unpossible")
        };

        (new_face_coord.clone(), (local_coord, new_dir))
    }

    fn wrap(&self, mut local_coord: Coord) -> Coord {
        local_coord.0 = modulo(local_coord.0, self.face_size as isize);
        local_coord.1 = modulo(local_coord.1, self.face_size as isize);
        local_coord
    }

    fn to_face(&self, mut pose: Pose) -> (Coord, Pose) {
        let face_coord = Coord(pose.0.0 / self.face_size as isize,
                               pose.0.1 / self.face_size as isize);
        pose.0 = self.wrap(pose.0);
        (face_coord, pose)
    }

    fn from_face(&self, face_coord: Coord, mut pose: Pose) -> Pose {
        pose.0 += face_coord * self.face_size as isize;
        pose
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

fn get_face(map: &CharGrid, face_coord: &Coord, face_size: usize) -> Face {
    use Tile::*;

    let face_size = face_size as isize;
    let mut tiles = HashMap::new();

    for x in 0..face_size {
        for y in 0..face_size {
            let local_coord = Coord(x, y);
            let global_coord = &local_coord + face_coord * face_size as isize;
            let c = map[global_coord.1 as usize][global_coord.0 as usize];
            tiles.insert(local_coord, match c {
                '#' => Wall,
                '.' => Empty,
                _ => panic!("no valid character at {}, {} in face {:?}", x, y, face_coord)});
        }
    }
    tiles
}

fn find_faces(map: CharGrid, face_size: usize) -> HashMap<Coord, Face> {
    let (width, height) = (map.iter().map(Vec::len).max().unwrap(), map.len());

    let mut faces = HashMap::new();
    for y in 0..height / face_size {
        for x in 0..width / face_size {
            if let Some(&c) = map[y * face_size].get(x * face_size) && c != ' ' {
                let face_coord = Coord(x as isize, y as isize);
                let face = get_face(&map, &face_coord, face_size);
                faces.insert(face_coord, face);
            }
        }
    }
    faces
}

mod mat {
    use std::mem::MaybeUninit;
    use std::ops::{Add, AddAssign, Index, IndexMut, Mul, Neg, Sub};

    pub struct Mat<T, const M: usize, const N: usize> ([T; M * N]) where [T; M * N]: Sized;

    impl<T, const M: usize, const N: usize> Mat<T, M, N>
    where [T; M*N]: Sized {
        pub fn new(data: [T; M*N]) -> Self {
            Mat(data)
        }

        pub fn empty() -> Mat<MaybeUninit<T>, M, N> {
            Mat(unsafe {
                MaybeUninit::uninit().assume_init()
            })
        }
    }

    impl<T, const M: usize, const N: usize> Mat<T, M, N>
    where [T; M*N]: Sized, [T; N*M]: Sized, T: Copy {
        pub fn transpose(&self) -> Mat<T, N, M> {
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
        pub fn identity() -> Self {
            let mut mat = Mat::<isize, M, M>::default();
            for i in 0..M {
                mat[(i, i)] = 1;
            }
            mat
        }
    }

    // MUL
    // Mat * Mat
    impl<T, const M: usize, const N: usize, const Q: usize> Mul<Mat<T, N, Q>> for Mat<T, M, N>
    where T: AddAssign + Default + Copy,
    for<'a, 'b> &'a T: Mul<&'b T, Output=T>,  // Should try to align this with parent Mul?
    [T; M*N]: Sized,
    [T; N*Q]: Sized,
    [T; M*Q]: Sized {
        type Output = Mat<T, M, Q>;
        fn mul(self, rhs: Mat<T, N, Q>) -> Self::Output {
            &self * &rhs
        }
    }

    // Mat * &Mat
    impl<T, const M: usize, const N: usize, const Q: usize> Mul<&Mat<T, N, Q>> for Mat<T, M, N>
    where T: AddAssign + Default + Copy,
        for<'a, 'b> &'a T: Mul<&'b T, Output=T>,  // Should try to align this with parent Mul?
        [T; M*N]: Sized,
        [T; N*Q]: Sized,
        [T; M*Q]: Sized {
        type Output = Mat<T, M, Q>;
        fn mul(self, rhs: &Mat<T, N, Q>) -> Self::Output {
            &self * rhs
        }
    }

    // &Mat * Mat
    impl<'a, T, const M: usize, const N: usize, const Q: usize> Mul<Mat<T, N, Q>> for &'a Mat<T, M, N>
    where T: AddAssign + Default + Copy,
        for<'b, 'c> &'c T: Mul<&'b T, Output=T>,  // Should try to align this with parent Mul?
        [T; M*N]: Sized,
        [T; N*Q]: Sized,
        [T; M*Q]: Sized {
        type Output = Mat<T, M, Q>;
        fn mul(self, rhs: Mat<T, N, Q>) -> Self::Output {
            self * &rhs
        }
    }

    // &Mat * &Mat
    impl<'a, T, const M: usize, const N: usize, const Q: usize> Mul<&'a Mat<T, N, Q>> for &'a Mat<T, M, N>
    where T: AddAssign + Default + Copy,
        for<'b> &'a T: Mul<&'b T, Output=T>,
        [T; M*N]: Sized,
        [T; N*Q]: Sized,
        [T; M*Q]: Sized {
        type Output = Mat<T, M, Q>;
        fn mul(self, rhs: &Mat<T, N, Q>) -> Self::Output {
            let mut mat = Mat::<T, M, Q>::empty();
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

    // ADD ASSIGN
    impl<T, const M: usize, const N: usize> AddAssign<&Self> for Mat<T, M, N>
    where [T; M*N]: Sized,
        for<'a> T: AddAssign<&'a T> {
        fn add_assign(&mut self, rhs: &Self) {
            for m in 0..M {
                for n in 0..N {
                    self[(m, n)] += &rhs[(m, n)];
                }
            }
        }
    }

    impl<T, const M: usize, const N: usize> AddAssign for Mat<T, M, N>
    where [T; M*N]: Sized,
        T: AddAssign {
        fn add_assign(&mut self, rhs: Self) {
            std::iter::IntoIterator::into_iter(rhs.0)
                .enumerate()
                .for_each(move |(i, x)| {self.0[i] += x;})
        }
    }

    // ADD
    impl<T, const M: usize, const N: usize> Add<&Self> for Mat<T, M, N>
    where [T; M*N]: Sized,
        for <'a> T: AddAssign<&'a T> {
        type Output = Self;
        fn add(mut self, rhs: &Self) -> Self::Output {
            self += rhs;
            self
        }
    }

    impl<T, const M: usize, const N: usize> Add for Mat<T, M, N>
    where [T; M*N]: Sized,
        for <'a> T: AddAssign<&'a T> {
        type Output = Self;
        fn add(self, rhs: Self) -> Self::Output {
            self + &rhs
        }
    }

    impl<T> Mat<T, 3, 3>
    where T: Default + Add + Mul + Neg<Output=T> + Copy {
        pub fn cross(x: (T, T, T)) -> Self {
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
        pub fn from_tuple(data: (T, T)) -> Self {
            Mat([data.0, data.1])
        }

        pub fn to_tuple(self) -> (T, T) {
            (self.0[0], self.0[1])
        }
    }

    impl<T: Copy> Mat<T, 1, 2> {
        pub fn from_tuple(data: (T, T)) -> Self {
            Mat([data.0, data.1])
        }

        pub fn to_tuple(self) -> (T, T) {
            (self.0[0], self.0[1])
        }
    }


    impl<T: Copy> Mat<T, 3, 1> {
        pub fn from_tuple(data: (T, T, T)) -> Self {
            Mat([data.0, data.1, data.2])
        }

        pub fn to_tuple(self) -> (T, T, T) {
            (self.0[0], self.0[1], self.0[2])
        }
    }

    impl<T: Copy> Mat<T, 1, 3> {
        pub fn from_tuple(data: (T, T, T)) -> Self {
            Mat([data.0, data.1, data.2])
        }

        pub fn to_tuple(self) -> (T, T, T) {
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

    pub fn cross<T>(a: (T, T, T), b: (T, T, T)) -> (T, T, T)
    where T: Mul<Output=T> + Sub<Output=T> + Copy {
        (a.1 * b.2 - a.2 * b.1,
        a.2 * b.0 - a.0 * b.2,
        a.0 * b.1 - a.1 * b.0)
    }
}

use mat::{Mat, cross};

fn cubify(face_coords: HashSet<Coord>) -> Edges {
    type V3 = (isize, isize, isize);
    let first_face = face_coords.iter().next().unwrap();
    let mut frontier: BTreeSet<&Coord> = BTreeSet::from([first_face]);
    let mut normals: HashMap<&Coord, V3> = HashMap::from([
        (first_face, (0, 0, 1))
    ]);
    let mut bases: HashMap<&Coord, Rc<Mat<isize, 2, 3>>> = HashMap::from([
        (first_face, Rc::new(Mat::new([1, 0, 0,
                                       0, 1, 0])))
    ]);

    let mut adjacent_normals: HashMap<&Coord, HashMap<Direction, V3>> = HashMap::new();

    while let Some(face) = frontier.pop_first() {
        let normal = normals[&face];
        let basis = Rc::clone(bases.get(face).unwrap());

        adjacent_normals.insert(&face, HashMap::new());

        for dir in Direction::DIRECTIONS.iter().copied() {
            let delta2d = Coord::from_direction(&dir);
            let delta3d: V3 = (Mat::<_, 1, 2>::from_tuple(delta2d.to_tuple()) * basis.deref()).to_tuple();
            adjacent_normals.get_mut(face).map(
                |normals| normals.insert(dir, delta3d));

            let new_face = face + delta2d;
            if let Some(new_face) = face_coords.get(&new_face) && !bases.contains_key(&new_face) {
                // 90 degree rotation of basis performed with Rodriguez' formula
                let i = Mat::<isize, 3, 3>::identity();
                let a = Mat::cross(cross(normal, delta3d));
                // It seems deref coercion doesn't work for &basis here - I
                // guess that deref coercion requires the target function and
                // its signature to be known a priori in order to figure that &T
                // is a valid substitution for invalid &Rc<T>; it cannot use the
                // non-existence of &Rc<T>.mul to trigger a search for &T.mul.
                let new_basis = basis.deref() * (i + &a*&a + a).transpose();
                bases.insert(new_face, Rc::new(new_basis));
                normals.insert(new_face, delta3d);
                frontier.insert(new_face);
            }
        }
    }

    // Invert the maps for reverse lookup
    let adjacent_dirs: HashMap<&Coord, HashMap<V3, Direction>> =
        adjacent_normals.iter().map(|(k, v)| {
            (*k, v.iter().map(|(k, v)| (*v, *k)).collect())
        }).collect();
    let faces: HashMap<V3, &Coord> = normals.iter().map(|(k, v)| (*v, *k)).collect();

    let mut resolved_edges: Edges = HashMap::new();
    for (face_a, dirs) in adjacent_normals.into_iter() {
        let normal_a = normals[face_a];
        for (dir_a, normal_b) in dirs.into_iter() {
            let face_b = faces[&normal_b];
            let dir_b = adjacent_dirs[face_b][&normal_a];
            resolved_edges.insert((face_a.clone(), dir_a), (face_b.clone(), dir_b));
        }
    }
    resolved_edges
}

fn wrap_edges(face_coords: HashSet<Coord>) -> Edges {
    let mut resolved_edges: Edges = HashMap::new();

    let max = face_coords.iter().fold(Coord::MIN, |c0, c1| c0.elementwise_max(c1));

    for fc in face_coords.iter() {
        for &direction in Direction::DIRECTIONS.iter() {
            let delta = Coord::from_direction(&direction);
            let mut neighbour = fc + &delta;
            if !face_coords.contains(&neighbour) {
                neighbour = match direction {
                    North => Coord(fc.0, max.1),
                    East => Coord(0, fc.1),
                    South => Coord(fc.0, 0),
                    West => Coord(max.0, fc.1),
                };
                while !face_coords.contains(&neighbour) {
                    neighbour += &delta;
                }
            }
            resolved_edges.insert((fc.clone(), direction),
                                  (neighbour, direction.opposite()));
        }
    }
    resolved_edges
}

fn main() {
    let face_size = 50;

    let mut lines: Vec<String> = std::io::stdin().lines().flat_map(|l| l).collect();
    let instructions = parse_instructions(&lines.pop().unwrap());
    let char_grid = lines_to_char_grid(lines);
    let faces = find_faces(char_grid, face_size);

    let edges = wrap_edges(faces.keys().cloned().collect());
    let mut map = Map::new(faces, edges, face_size);
    println!("{}", password(
        Instruction::execute_all(instructions.iter().cloned(), &map)));

    map.edges = cubify(map.faces.keys().cloned().collect());
    println!("{}", password(
        Instruction::execute_all(instructions.into_iter(), &map)));
}

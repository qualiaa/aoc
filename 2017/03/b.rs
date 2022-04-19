use std::io;
use std::ops::Add;
use std::ops::AddAssign;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Point(i32, i32);

const LEFT: Point = Point(-1, 0);
const RIGHT: Point = Point(1, 0);
const UP: Point = Point(0, 1);
const DOWN: Point = Point(0, -1);

impl Point {
    fn moore_neighbours(&self) -> Vec<Point> {
        vec!(self + &UP, self + &DOWN, self + &LEFT, self + &RIGHT,
             self + &(&UP + &RIGHT),
             self + &(&UP + &LEFT),
             self + &(&DOWN + &RIGHT),
             self + &(&DOWN + &LEFT))  // Is there a way to make this implicit?
                                       // Alternatively implement Add<Point> for &Point
    }

    fn rotate(&mut self) {
        let x = self.0;
        self.0 = -self.1;
        self.1 = x;
    }
}

impl Add<i32> for &Point {
    type Output = Point;
    fn add(self, other: i32) -> Point {
        Point(self.0 + other, self.1 + other)
    }
}
impl Add<&Point> for &Point {
    type Output = Point;
    fn add(self, other: &Point) -> Point {
        Point(self.0 + other.0, self.1 + other.1)
    }
}
impl AddAssign<&Point> for Point {
    fn add_assign(&mut self, other: &Point) {
        self.0 += other.0;
        self.1 += other.1;
    }
}

fn main() {
    let mut buf = String::new();
    io::stdin().read_line(&mut buf);
    let target_value: u32 = buf.trim().parse().unwrap();

    let mut visited = HashMap::from([(Point(0, 0), 1u32)]);

    let mut dir = Point(1, 0);
    let mut position = Point(0, 0);
    let mut side_length = 2;

    let result = 'outer: loop {
        for side in 0..4 {
            // Iterate over a single spiral radius
            if side != 0 {
                dir.rotate();
            }
            for step in 0..side_length {
                // Iterate over each step along a single side
                if side == 0 && step == 1 {
                    dir.rotate();
                }

                position += &dir;
                let value = calculate_value(&visited, &position);
                if value > target_value {
                    break 'outer value;
                }
                visited.insert(position.clone(), value);
            }
        }
        side_length += 2
    };
    println!("{}", result);
}

fn calculate_value(visited: &HashMap<Point, u32>, position: &Point) -> u32 {
    position.moore_neighbours().iter()
        .filter_map(|p| visited.get(p))
        .sum()
}

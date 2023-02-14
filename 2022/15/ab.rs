use std::ops::RangeInclusive;
use std::str::FromStr;
use std::cmp;

#[derive(Debug)]
struct Circle {
    x: isize,
    y: isize,
    r: isize
}

impl Circle {
    fn from_point_on_circumference(x: isize, y: isize, x1: isize, y1: isize) -> Self{
        Circle {x, y, r: (x-x1).abs()+(y-y1).abs()}
    }

    fn intersections_at(&self, y: isize) -> Option<RangeInclusive<isize>> {
        if (y-self.y).abs() <= self.r {
            let a = self.r - (self.y - y).abs();
            Some((self.x-a)..=(self.x+a))
        } else {
            None
        }
    }
}

impl FromStr for Circle {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, <Self as FromStr>::Err> {
        let cs: Vec<isize> = s.split_ascii_whitespace()
            .map(|w| w.chars().filter(|&c| c == '-' || c.is_numeric()).collect())
            .filter_map(|digits: String| digits.parse().ok())
            .collect();

        if cs.len() == 4 {
            Ok(Circle::from_point_on_circumference(cs[0], cs[1], cs[2], cs[3]))
        } else {
            Err(())
        }
    }
}

fn join_ranges<T: Copy + Ord>(r1: &RangeInclusive<T>, r2: &RangeInclusive<T>) -> RangeInclusive<T>{
    let &start = cmp::min(r1.start(), r2.start());
    let &end = cmp::max(r1.end(), r2.end());
    start..=end
}

fn overlapping<T: Ord>(r1: &RangeInclusive<T>, r2: &RangeInclusive<T>) -> bool {
    r1.contains(&r2.start())
        || r1.contains(&r2.end())
        || r2.contains(&r1.start())
        || r2.contains(&r1.end())
}

fn combine_ranges(ranges: &[RangeInclusive<isize>]) -> Vec<RangeInclusive<isize>> {
    let mut result = Vec::new();
    let mut ranges = Vec::from(ranges);
    while let Some(mut range) = ranges.pop() {
        while let Some(i) = ranges.iter().position(|r| overlapping(&range, &r)) {
            range = join_ranges(&range, &ranges.swap_remove(i));
        }
        result.push(range);
    }
    result
}

fn main() {
    let circles: Vec<Circle> = std::io::stdin().lines()
        .filter_map(Result::ok)
        .map(|l| l.parse::<Circle>().unwrap())
        .collect();

    let ranges_at = |y|->Vec<_> {circles.iter().filter_map(|c| c.intersections_at(y)).collect()};

    let joined_ranges = combine_ranges(&ranges_at(2_000_000));
    println!("{}", joined_ranges.iter().fold(0, |a, r| a + r.end()-r.start()));

    let size = 4_000_000;
    for y in 0..=size {
        let joined_ranges = combine_ranges(&ranges_at(y));
        for range in joined_ranges.iter() {
            if (1..=size).contains(range.start()) {
                let x = range.start() - 1;
                println!("{}", x*size+ y);
            }
        }
    }
}

use std::str::FromStr;

struct SectorRange {
    start: u32,
    end: u32
}

impl SectorRange {
    fn fully_contains(&self, other: &Self) -> bool {
        self.start <= other.start && other.end <= self.end
    }
}

fn total_overlap (a: &SectorRange, b: &SectorRange) -> bool {
    a.fully_contains(b) || b.fully_contains(a)
}

impl FromStr for SectorRange {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.split_once('-').ok_or(()).and_then(|(start, end)| {
            start.parse().and_then(|start| {
                end.parse().map(|end| {
                    SectorRange {start, end}
                })
            }).map_err(|_| ())
        })
    }
}

fn main() {
    let result: u32 = std::io::stdin().lines()
        .map(|l| l.unwrap())
        .map(|l| {
            l.split_once(',').ok_or(()).and_then(|(a, b)| {
                a.parse().and_then(|a| {
                    b.parse().map(|b| {
                        total_overlap(&a, &b) as u32
                    })
                }).map_err(|_| ())
            }).unwrap()
        }).sum();
    println!("{}", result)
}

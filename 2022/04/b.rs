use std::str::FromStr;

struct SectorRange {
    start: u32,
    end: u32
}

fn any_overlap (a: &SectorRange, b: &SectorRange) -> bool {
    a.start <= b.start && a.end >= b.start || b.start <= a.start && b.end >= a.start
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
                        any_overlap(&a, &b) as u32
                    })
                }).map_err(|_| ())
            }).unwrap()
        }).sum();
    println!("{}", result)
}

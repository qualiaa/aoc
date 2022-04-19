use std::io;
use std::io::BufRead;

fn main() {
    let stdin = io::stdin();
    let lines = stdin.lock().lines();
    // FIXME: Couldn't work out how to specify the result type for sum()
    let sum: u32 = lines.filter_map(|x| x.map(process).ok()).sum();
    println!("{}", sum)
}

fn process(line: String) -> u32 {
    let (min, max) = line.split_whitespace()
        .map(str::parse)
        .filter_map(Result::ok)  // XXX: Is this the best way of doing this?
        .fold((u32::MAX, u32::MIN), |a, x: u32| (x.min(a.0), x.max(a.1)));
    max-min
}

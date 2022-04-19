use std::io;
use std::io::BufRead;

fn main() {
    let stdin = io::stdin();
    let input_lines = stdin.lock().lines();
    for result in input_lines.map(|l| l.map(process)) {
        if let Ok(sum) = result {
            println!("{}", sum);
        }
    }
}

fn process(line: String) -> u32 {
    let (start, mut middle) = (line.chars(), line.chars().cycle());
    middle.nth(line.len()/2 - 1);

    start.zip(middle)
        .filter(|(x, y)| x==y)
        .map(|(x, _)| x.to_digit(10).unwrap())
        .sum()
}

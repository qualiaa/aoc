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

fn process(mut line: String) -> u32 {
    line.push(line.chars().next().unwrap());
    let (first, mut second) = (line.chars(), line.chars());
    second.next();

    first.zip(second)
        .filter(|(x, y)| x==y)
        .map(|(x, _)| x.to_digit(10).unwrap())
        .sum()
}

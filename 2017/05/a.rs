use std::io;
use std::io::BufRead;

fn main() {
    let stdin = io::stdin();
    let lines = stdin.lock().lines();

    let mut jumps: Vec<i32> = lines
        .filter_map(Result::ok)
        .filter_map(|l| l.trim().parse().ok())
        .collect();
    let mut index: i32 = 0;
    let mut num_steps: u32 = 0;
    while index >= 0 && (index as usize) < jumps.len() {
        let inst = &mut jumps[index as usize];
        index += *inst;
        *inst += 1;
        num_steps += 1
    }
    println!("{}", num_steps);
}

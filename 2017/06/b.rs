use std::io;
use std::collections::HashMap;

fn main() {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    let banks = input.split_whitespace()
        .filter_map(|s| s.parse().ok())
        .collect();
    println!("{:?}", run_reallocations(banks));
}

fn run_reallocations(mut banks: Vec<usize>) -> usize {
    let mut seen_configurations = HashMap::new();

    loop {
        if let Some(step) = seen_configurations.insert(banks.clone(), seen_configurations.len()) {
            break seen_configurations.len()-step;
        }

        let (bank_index, &blocks) =
            banks.iter().enumerate()
                        .reduce(|a@(_, n), x@(_, m)| if m > n {x} else {a}).unwrap();

        banks[bank_index] = 0;
        for i in 1..=blocks {
            let j = (i + bank_index) % banks.len();
            banks[j] += 1;
        }
    }
}

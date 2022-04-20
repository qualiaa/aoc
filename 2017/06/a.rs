use std::io;
use std::collections::HashSet;

fn main() {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    let banks = input.split_whitespace()
        .filter_map(|s| s.parse().ok())
        .collect();
    println!("{}", run_reallocations(banks));
}

fn run_reallocations(mut banks: Vec<usize>) -> usize {
    let mut seen_configurations = HashSet::new();

    loop {
        seen_configurations.insert(banks.clone());

        let (bank_index, &blocks) =
            banks.iter().enumerate()
                        .reduce(|a@(_, n), x@(_, m)| if m > n {x} else {a}).unwrap();

        banks[bank_index] = 0;
        for i in 1..=blocks {
            let j = (i + bank_index) % banks.len();
            banks[j] += 1;
            // XXX: Why does this fail borrow checking?
            // banks[(i + bank_index + 1) % banks.len()] += 1;
        }
        if seen_configurations.contains(&banks) {
            break seen_configurations.len();
        }
    }
}

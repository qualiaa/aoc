use std::io;
use std::io::BufRead;
use std::collections::HashMap;

fn main() {
    let stdin = io::stdin();
    let lines = stdin.lock().lines();

    println!("{}", lines
             .filter_map(|l| l.map(valid_passphrase).ok())
             .filter(|x| *x)
             .count());
}

fn valid_passphrase(phrase: String) -> bool {
    let mut passed = Vec::new();
    for letter_counts in phrase.split_whitespace().map(letter_counts) {
        if passed.contains(&letter_counts) {
            return false;
        }
        passed.push(letter_counts);
    }
    true
}

fn letter_counts(word: &str) -> HashMap<char, u32> {
    let mut counts = HashMap::new();
    for c in word.chars() {
        counts.entry(c).and_modify(|x| *x+=1).or_insert(1 as u32);
    }
    counts
}

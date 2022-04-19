use std::io;
use std::io::BufRead;
use std::collections::HashSet;

fn main() {
    let stdin = io::stdin();
    let lines = stdin.lock().lines();

    println!("{}",
             lines
             .filter_map(|line| line.map(valid_passphrase).ok())
             .filter(|x| *x)
             .count());
}

fn valid_passphrase(phrase: String) -> bool {
    let num_words = phrase.split_whitespace().count();
    let set: HashSet<&str> = phrase.split_whitespace().collect();
    set.len() == num_words
}

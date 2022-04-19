use std::io;
use std::io::BufRead;

fn main() {
    let stdin = io::stdin();
    let lines = stdin.lock().lines();
    let sum: u32 = lines.filter_map(|x| x.map(process).ok()).sum();
    println!("{}", sum)
}

fn process(line: String) -> u32 {
    let values: Vec<u32> = line.split_whitespace()
        .map(str::parse)
        .filter_map(Result::ok)
        .collect();
    let iters: Vec<_> = (0..values.len())
        .map(|i| {let mut iter = values.iter().cycle();
                  // iter.advance_by(i) in nightly is nicer
                  if i != 0 {iter.nth(i-1);()};
                  iter})
        .collect();
    let nexts = (0..values.len())
        .map(|i| iters.iter().map(move |iter| iter.clone().nth(i).unwrap()));
    for mut row in nexts {
        let first_value = row.next().unwrap();
        let result = row.find_map(|x|
                                  if first_value % x == 0 {Some(first_value/x)}
                                  else {None});
        if let Some(x) = result {
            return x;
        }
    }
    panic!("Didn't find shit")
}

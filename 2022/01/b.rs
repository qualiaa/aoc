use std::iter::Iterator;
use std::vec::IntoIter;

fn extract_while<I, P>(iter: &mut I, mut predicate: P) -> Vec<I::Item>
    where
        I: Iterator,
        P: FnMut(&I::Item) -> bool {
    let mut data = Vec::new();
    while let Some(x) = iter.next() {
        if predicate(&x) {
            data.push(x)
        } else {
            break;
        }
    }
    data
}

struct Disperse<I, P> {
    iter: I,
    predicate: P
}

impl<I, P> Disperse<I, P> {
    fn new(iter: I, predicate: P) -> Disperse<I, P> {
        Disperse {iter, predicate}
    }
}

impl<I: Iterator, P: FnMut(&I::Item)->bool> Iterator for Disperse<I, P> {
    type Item = IntoIter<I::Item>;

    fn next(&mut self) -> Option<Self::Item> {
        let p = &mut self.predicate;
        if let Some(x) = self.iter.next() {
            let result;
            if p(&x) {
                result = Vec::new().into_iter();
            } else {
                let mut data = vec![x];
                data.extend(extract_while(&mut self.iter, |x| !p(&x)).into_iter());
                result = data.into_iter();
            }
            Some(result)
        } else {
            None
        }
    }
}

fn main() {
    let lines = std::io::stdin()
        .lines()
        .map(|x| x.unwrap().parse::<u32>());
    let groups = Disperse::new(lines, |x: &Result<u32, _>| !x.is_ok());
    let max: u32 = groups.map(|c| c.into_iter().map(|x| x.unwrap()).sum()).max().unwrap();
    println!("{}", max);
}

use std::collections::HashSet;
use std::convert::TryFrom;
use std::iter::{FromIterator, Iterator};
use std::vec::IntoIter;

fn priority(c: char) -> Option<u8> {
    u8::try_from(c).ok().and_then(|ord| match c {
        'A'..='Z' => Some(ord - 64 + 26),
        'a'..='z' => Some(ord - 96),
        _ => None
    })
}

struct ChunksOf<I, const N: usize> {
    iter: I,
    done: bool
}

impl<I, const N: usize> ChunksOf<I, N> {
    fn new(iter: I) -> ChunksOf<I, N> {
        ChunksOf {iter, done: false}
    }
}

impl<I: Iterator, const N: usize> Iterator for ChunksOf<I, N> {
    //type Item = Box<dyn Iterator>;
    type Item = IntoIter<I::Item>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            None
        } else {
            let result: Vec<I::Item> = (&mut self.iter).take(N).collect();
            self.done = result.len() < N;
            if !result.is_empty() { Some(result.into_iter()) } else { None }
        }
    }
}

trait Chunkable<I> {
    fn chunks_of<const N: usize>(self) -> ChunksOf<I, N>;
}
impl<I: Iterator> Chunkable<I> for I {
    fn chunks_of<const N: usize>(self) -> ChunksOf<I, N> {
        ChunksOf::new(self)
    }
}

//impl<I: Iterator> Iterator for ChunksOf<I> {
//    type Item = IntoIter<I::Item>;
//    fn next(&mut self) -> Option<Self::Item> {
//        if self.done {
//            None
//        } else {
//            let mut result = Vec::new();
//            for _ in 0..self.n {
//                if let Some(x) = self.iter.next() {
//                    result.push(x);
//                } else {
//                    self.done = true;
//                    break
//                }
//            }
//            if !result.is_empty() {
//                Some(result.into_iter())
//            } else {
//                None
//            }
//        }
//    }
//}

fn main() {
    let result: u32 = std::io::stdin()
        .lines().map(|l| l.unwrap())
        .chunks_of::<3>()  // apparently array_chunks exists... whoops
        .map(|group|
             group.map(|l| HashSet::from_iter(l.chars()))
             .reduce(|x , y| -> HashSet<char> {
                 HashSet::from_iter(x.intersection(&y).cloned())})
             .and_then(|c| c.into_iter().next())
             .and_then(priority)
             .unwrap() as u32
        ).sum();
    println!("{}", result);
}

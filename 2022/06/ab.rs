use std::collections::HashMap;
use std::hash::Hash;
use std::iter::{FromIterator, zip};
use std::ops::{AddAssign, SubAssign};
use std::io::Read;

struct Counter<T> {
    counts: HashMap<T, usize>
}

impl<T: Eq + Hash + Clone> FromIterator<T> for Counter<T> {
    fn from_iter<I: IntoIterator<Item=T>>(iter: I) -> Self {
        let mut new = Self::new();
        for v in iter {
            new += &v;
        }
        new
    }
}

impl<T: Eq + Hash + Clone> AddAssign<&T> for Counter<T> {
    fn add_assign(&mut self, rhs: &T) {
        self.counts.entry(rhs.clone()).and_modify(|n| { *n+=1; }).or_insert(1);

    }
}

impl<T: Eq + Hash> SubAssign<&T> for Counter<T> {
    fn sub_assign(&mut self, rhs: &T) {
        if 0 == *self.counts.get_mut(rhs).map(|n| {*n-=1; n}).unwrap_or(&mut 1) {
            self.counts.remove(rhs);
        }
    }
}

impl<T: Eq + Hash> Counter<T> {
    fn new() -> Self {
        Counter { counts: HashMap::new() }
    }

    fn contains_duplicates(&self) -> bool {
        self.counts.values().copied().max().unwrap_or(0) > 1
    }
}

fn find_packet(len: usize, data: &Vec<u8>) -> Option<usize> {
    let (old_data, mut new_data) = (data.iter(), data.iter());
    let mut packet: Counter<_> = new_data.by_ref().take(len-1).collect();

    for (i, (rem, add)) in zip(old_data, new_data).enumerate() {
        packet += &add;
        if !packet.contains_duplicates() {
            return Some(i + len);
        }
        packet -= &rem;
    }
    None
}

fn main() {
    let data = std::io::stdin().bytes().map(|b| b.unwrap()).collect();
    println!("{}", find_packet(4, &data).unwrap());
    println!("{}", find_packet(14, &data).unwrap());
}

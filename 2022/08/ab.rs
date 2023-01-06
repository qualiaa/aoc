use std::collections::{HashMap, HashSet};
use std::hash::Hash;

type Height = isize;
type Coord = (usize, usize);

fn visible_indices<T>(it: impl Iterator<Item=(T, Height)>) -> impl Iterator<Item=T> {
    // I struggle to believe rust's scan implementation is this bad.
    // If it were sensible, the options would not have to be doubly-wrapped, and
    // the filter_map would be unnecessary.
    it.scan(-1, |max: &mut Height, (i, h)|
            if h > *max {*max = h; Some(Some(i))} else {Some(None)} // lmao
    ).filter_map(std::convert::identity)
}

fn look_both_ways<I, T>(i: I) -> impl Iterator<Item=T>
    where
        I: Iterator<Item=(T, Height)> + Clone + DoubleEndedIterator {
    visible_indices(i.clone()).chain(visible_indices(i.rev()))
}

fn to_hashmap<T>(v: Vec<Vec<T>>) -> HashMap<Coord, T> {
    v.into_iter().enumerate().flat_map(
        move |(i, row)| row.into_iter().enumerate().map(
            move |(j, x)| ((i, j), x)
        )).collect()
}

fn from_spot<K>(trees: &HashMap<K, Height>, max: Height, ks: impl Iterator<Item=K>) -> usize
    where K: Eq + Hash {
    let mut count = 0;
    for k in ks {
        count += 1;
        if trees[&k] >= max {
            break
        }
    }
    count
}

fn main() {
    let trees: Vec<Vec<Height>> = std::io::stdin().lines().map(|l| {
        l.unwrap()
            .chars()
             // Not sure if there's a better way to do this
            .map(|c| String::from(c).parse::<Height>().unwrap())
            .collect()
    }).collect();

    let N = trees.len();
    let left_right = trees.iter().enumerate().flat_map(|(i, row)| {
        look_both_ways(row.iter().copied().enumerate())
            .map(move |j| (i, j))
    });
    let up_down = (0..N).flat_map(|j| {
        look_both_ways(trees.iter().map(move |row| row[j]).enumerate())
            .map(move |i| (i, j))
    });
    println!("{}", left_right.chain(up_down).collect::<HashSet<_>>().len());

    let trees = to_hashmap(trees);
    println!("{}", trees.iter().map(|((i, j), max)| {
        from_spot(&trees, *max, (i+1..N).map(|i| (i, *j)))
            * from_spot(&trees, *max, (0..*i).rev().map(|i| (i, *j)))
            * from_spot(&trees, *max, (j+1..N).map(|j| (*i, j)))
            * from_spot(&trees, *max, (0..*j).rev().map(|j| (*i, j)))
    }).max().unwrap())
}

use std::collections::VecDeque;

fn mix(mut is: VecDeque<usize>, vs: &[isize]) -> VecDeque<usize> {
    let n = vs.len() as isize - 1; // Number of unique places to insert item.

    for (i, &shift) in vs.iter().enumerate() {
        let j = is.iter().position(|&i_| i_==i).unwrap();
        is.remove(j);
        let mut j = (j as isize + shift) % n;
        if j <= 0 {
            j = n + j; // If < 0, wrap around; if at beginning, put at end.
        }
        is.insert(j as usize, i);
    }
    is
}

fn coord_sum(vs: &[isize]) -> isize {
    let zero_index = vs.iter().position(|&v| v==0).unwrap();
    [1000, 2000, 3000].iter()
        .map(|&i: &usize| (zero_index + i) % vs.len())
        .map(|i| vs[i]).sum()
}

fn argsort(vs: Vec<isize>, is: impl Iterator<Item=usize>) -> Vec<isize> {
    is.into_iter().map(|i| vs[i]).collect()
}

fn main() {
    let vs: Vec<isize> = std::io::stdin().lines()
        .flat_map(|x|x)
        .filter_map(|l|l.parse().ok())
        .collect();
    let decrypted: Vec<isize> = vs.iter().map(|&x| x*811589153).collect();

    let is = mix((0..vs.len()).collect(), &vs);
    println!("{}", coord_sum(&argsort(vs, is.into_iter())));

    let mut is: VecDeque<usize> = (0..decrypted.len()).collect();
    for _ in 0..10 {
        is = mix(is, &decrypted);
    }

    println!("{}", coord_sum(&argsort(decrypted, is.into_iter())));
}

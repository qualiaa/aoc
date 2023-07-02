fn fuck_up(v: isize) -> String {
    if v == 0 {
        return String::from("");
    }
    let (q, r) = (v / 5, v % 5);
    let carry = (r>2) as isize;

    format!("{}{}", fuck_up(q + carry), match r - carry * 5 {
        -2 => '=', -1 => '-', r => char::from_digit(r as u32, 3).unwrap()
    })
}

fn main() {
    println!("{}", fuck_up(
        std::io::stdin()
            .lines()
            .flat_map(|l| l)
            .map(|l|
                 l.chars()
                 .map(|c| match c {
                     '=' => -2, '-' => -1, c => c.to_digit(3).unwrap() as isize})
                 .rev()
                 .enumerate()
                 .map(|(i, v)| 5isize.pow(i as u32) * v)
                 .sum::<isize>()
            ).sum::<isize>()))
}

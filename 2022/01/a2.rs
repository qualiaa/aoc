fn main() {
    fn go(mut a: Vec<Vec<u32>>, x: String) -> Vec<Vec<u32>> {
        if let Ok(x) = x.parse::<u32>() {
            a.last_mut().unwrap().push(x);
        } else {
            a.push(Vec::new());
        }
        a
    }

    let data = std::io::stdin().lines()
        .map(|x| x.unwrap())
        .fold(vec![Vec::new()], go);
    let result: u32 = data.iter().map(|x| x.iter().sum()).max().unwrap();
    println!("{}", result);
}

use std::collections::HashMap;
use std::mem::take;
use std::str::FromStr;

enum Operation {Plus, Times, Minus, Divide}
use Operation::*;
impl Operation {
    fn result(&self, x: isize, y: isize) -> isize {
        match self {
            Plus => x + y,
            Times => x * y,
            Minus => x - y,
            Divide => x / y
        }
    }
}

impl FromStr for Operation {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, <Self as FromStr>::Err> {
        match &s[..] {
            "+" => Ok(Plus),
            "*" => Ok(Times),
            "-" => Ok(Minus),
            "/" => Ok(Divide),
            _ => Err(())
        }
    }
}

enum Value {
    Literal(isize),
    Expression(String, Operation, String)
}
use Value::*;

impl FromStr for Value {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, <Self as FromStr>::Err> {
        let words: Vec<&str> = s.trim().split_ascii_whitespace().collect();
        let fail = Err(());
        match &words[..] {
            &[x, op, y] => op.parse::<Operation>().map(|op| Expression(x.into(), op, y.into())).or(fail),
            [x] => x.parse::<isize>().map(Literal).or(fail),
            _ => fail
        }
    }
}

impl Value {
    fn resolve(&self, symbol_table: &HashMap<String, Value>) -> isize {
        match self {
            Literal(x) => *x,
            Expression(x, op, y) => op.result(symbol_table[x].resolve(&symbol_table),
                                              symbol_table[y].resolve(&symbol_table))
        }
    }
}

fn parse_pair(s: &str) -> (String, Value) {
    let (k, v) = s.split_once(':').unwrap();
    (k.into(), v.parse().unwrap())
}

fn minimise(mut f: impl FnMut(isize) -> isize, mut x: isize, mut delta_x: isize, lr: f64) -> isize {
    loop {
        let y = f(x);
        if y == 0 { return x }
        x += delta_x;
        let delta_y = f(x) - y;
        if delta_y == 0 {continue}
        let tmp = y as f64 * delta_x as f64 / delta_y as f64;
        delta_x = -(lr * tmp) as isize;
        if delta_x == 0 {
            delta_x = -tmp.signum() as isize;
        }
    }
}

fn main() {
    let mut symbol_table = std::io::stdin().lines()
        .flat_map(|l| l)
        .map(|s| parse_pair(&s))
        .collect::<HashMap<String, Value>>();

    println!("{}", symbol_table["root"].resolve(&symbol_table));

    let root: &mut Value = &mut symbol_table.get_mut("root").unwrap();
    *root = match root {
        Expression(x, _, y) => Expression(take(x), Minus, take(y)),
        _ => panic!("Uh oh")
    };

    println!("{}", minimise(|x| {
        symbol_table.insert("humn".into(), Literal(x));
        symbol_table["root"].resolve(&symbol_table).abs()
    }, 0, 10, 0.1))
}

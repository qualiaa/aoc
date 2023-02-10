mod scan {
    use std::collections::VecDeque;
    use std::error::Error;
    use std::iter::{FromIterator, empty};
    use std::fmt;

    // A different tack for parsing: a scanner with a low memory footprint which can
    // be lazy in its input. At some point I'll have another crack at parser
    // combinators...
    pub struct Scanner {
        string_feed: Box<dyn Iterator<Item=String>>,
        // I considered using a Box<impl Iterator<Item=char>> here... still not sure
        // which I'd prefer. Would require maintaining another buffer for lookahead.
        // This way, we store slightly more in memory than we need and have to
        // maintain a local cursor, but otherwise it's quite clean.
        loaded: VecDeque<char>,
        cursor: usize,
        pos: ScanPos
    }

    #[derive(Debug, Clone)]
    struct ScanPos {
        chars: usize,
        line: usize,
        col: usize
    }

    impl ScanPos {
        fn new() -> Self {
            ScanPos {chars: 1, line: 1, col: 1}
        }

        fn proceed(&mut self, c: char) {
            if c == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.chars += 1;
        }
    }

    impl fmt::Display for ScanPos {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}:{}:{}", self.chars, self.line, self.col)
        }
    }

    #[derive(Debug)]
    pub struct ParseError {
        pos: ScanPos,
        msg: String,
        cause: Option<Box<dyn Error>>
    }

    impl fmt::Display for ParseError {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "Parse error {}: {}", self.pos, self.msg)
        }
    }
    impl Error for ParseError {
        fn cause(&self) -> Option<&(dyn Error + 'static)> {
            self.cause.as_deref()
        }
    }

    pub type ParseResult<T> = Result<T, ParseError>;

    impl Scanner {
        pub fn strict(input: &str) -> Self {
            Scanner {
                string_feed: Box::new(empty()),
                loaded: input.chars().collect(),
                cursor: 0,
                pos: ScanPos::new()
            }
        }

        pub fn lazy<SF: Iterator<Item=String> + 'static>(input: SF) -> Self {
            Scanner{
                string_feed: Box::new(input),
                loaded: VecDeque::new(),
                cursor: 0,
                pos: ScanPos::new()
            }
        }

        pub fn peek_by(&mut self, i: usize) -> ParseResult<char> {
            if let Some(c) = self.loaded.get(self.cursor + i).copied() {
                Ok(c)
            } else if let Some(string) = self.string_feed.next() {
                self.loaded = self.loaded.split_off(self.cursor);
                self.loaded.extend(string.chars());
                self.cursor = 0;
                self.peek_by(i)
            } else {
                Err(self.fail(&format!("input exhausted at offset {}", i)))
            }
        }

        pub fn fail(&self, msg: &str) -> ParseError {
            ParseError { pos: self.pos.clone(), msg: String::from(msg), cause: None}
        }

        pub fn fail_from<E: Error + 'static>(&self, cause: E, msg: &str) -> ParseError {
            ParseError { cause: Some(Box::new(cause)), ..self.fail(msg) }
        }

        pub fn peek(&mut self) -> ParseResult<char> {
            self.peek_by(0)
        }

        pub fn next(&mut self) -> ParseResult<char> {
            self.peek().map(|c| {self.cursor += 1; self.pos.proceed(c); c })
        }

        pub fn eof(&mut self) -> ParseResult<()> {
            self.peek().and(
                Err(self.fail("EOF expected"))
            ).or(Ok(()))
        }

        pub fn skip(&mut self) -> ParseResult<()> {
            self.next().and(Ok(()))
        }

        pub fn skip_by(&mut self, n: usize) -> ParseResult<()> {
            for _ in 0..n {
                self.skip()?;
            }
            Ok (())
        }

        pub fn satisfy(&mut self, p: impl FnOnce(&char) -> bool) -> ParseResult<char> {
            match self.peek() {
                Ok(c) if p(&c) => self.next(),
                Err(e) => Err(self.fail_from(e, "failed while matching predicate")),
                _ => Err(self.fail("predicate not satisfied"))
            }
        }

        pub fn next_is(&mut self, c: char) -> ParseResult<char> {
            self.satisfy(|x| x==&c).map_err(
                |e| self.fail_from(e, &format!("failed while seeking `{}'", c)))
        }

        pub fn string(&mut self, s: &str) -> ParseResult<String> {
            let cursor = self.cursor;
            for (i, c) in s.chars().enumerate() {
                match self.peek_by(i) {
                    Ok(x) if c != x => Err(self.fail(&format!("could not match string `{}'", s))),
                    Err(e) => Err(self.fail_from(e, &format!("failed while matching {}", s))),
                    _ => Ok(())
                }?
            }
            self.skip_by(s.len())?;
            Ok(String::from(s))
        }

        pub fn some(&mut self, mut p: impl FnMut(&char) -> bool) -> ParseResult<String> {
            let mut cs = Vec::new();
            while let Ok(c) = self.peek() {
                if !p(&c) {
                    break
                }
                cs.push(self.next().unwrap());
            }
            if cs.is_empty() {
                self.peek().map_err(|e| self.fail_from(e, "while matching predicate"))
                    .and(Err(self.fail("predicate could not match any chars")))
            } else {
                Ok(String::from_iter(cs))
            }
        }

        pub fn many(&mut self, p: impl FnMut(&char) -> bool) -> ParseResult<String> {
            self.some(p).or(Ok(String::new()))
        }

        pub fn skip_many(&mut self, p: impl FnMut(&char) -> bool) -> ParseResult<()> {
            self.many(p).and(Ok(()))
        }

        pub fn skip_some(&mut self, p: impl FnMut(&char) -> bool) -> ParseResult<()> {
            self.some(p).and(Ok(()))
        }

        pub fn natural(&mut self) -> ParseResult<usize> {
            self.some(char::is_ascii_digit).map(|x| x.parse().unwrap())
        }

        pub fn integer(&mut self) -> ParseResult<isize> {
            let neg = self.next_is('-').is_ok();
            let n = self.natural()? as isize;
            Ok(if neg {- n} else {n})
        }
    }
}

// Disable Copy (hence implicit pass-by-value) to get compiler guarantees that
// moving moves.
struct Item {worry: usize}

enum Operand {Old, Num(usize)}
use Operand::*;
impl Operand {
    fn resolve(&self, old: usize) -> usize {
        match self {
            Old => old,
            Num(x) => *x
        }
    }
}

enum Operation {
    Add(Operand, Operand),
    Mul(Operand, Operand)
}
use Operation::*;

impl Operation {
    fn call(&self, i: &mut Item) {
        i.worry = self.value(i.worry)
    }

    fn value(&self, old: usize) -> usize {
        match self {
            Add(x, y) => x.resolve(old) + y.resolve(old),
            Mul(x, y) => x.resolve(old) * y.resolve(old)
        }
    }
}

struct Test(usize);
impl Test {
    fn as_fn(self) -> impl FnMut(&Item) -> bool {
        move |i| i.worry % self.0 == 0
    }
}

struct Monkey {
    items: Vec<Item>,
    operation: Operation,
    test: Box<dyn FnMut(&Item) -> bool>,
    targets: [usize; 2]
}

impl Monkey {
    fn pass(&mut self, i: Item) {
        self.items.push(i)
    }

    fn update(&mut self) -> Vec<(usize, Item)> {
        let items: Vec<_> = self.items.drain(..).collect();
        items.into_iter().map(
            |mut item| {
                self.operation.call(&mut item);
                item.worry /= 3;
                (self.targets[(self.test)(&item) as usize], item)
            }).collect()
    }
}

use scan::{ParseResult,Scanner};

fn parse_item(s: &mut Scanner) -> ParseResult<Item> {
    Ok(Item {worry: s.integer()? as usize})
}

fn parse_item_list(s: &mut Scanner) -> ParseResult<Vec<Item>> {
    let mut items = vec![parse_item(s)?];

    while let Ok(_) = s.string(", ") {
        items.push(parse_item(s)?)
    }
    Ok(items)
}

fn parse_test(s: &mut Scanner) -> ParseResult<impl FnMut(&Item) -> bool> {
    s.string("divisible by ")?;
    let divisor = s.integer()? as usize;
    Ok(Test(divisor).as_fn())
}

fn parse_operand(s: &mut Scanner) -> ParseResult<Operand> {
    if let Ok(_) = s.string("old") {
        Ok(Operand::Old)
    } else if let Ok(n) = s.integer() {
        Ok(Operand::Num(n as usize))
    } else {
        Err(s.fail("invalid operand"))
    }
}

fn parse_operation(s: &mut Scanner) -> ParseResult<Operation> {
    s.string("new = ")?;
    let op1 = parse_operand(s)?;
    s.skip_many(char::is_ascii_whitespace)?;
    let operator = s.next()?;
    s.skip_many(char::is_ascii_whitespace)?;
    let op2 = parse_operand(s)?;
    match operator {
        '+' => Ok(Add(op1, op2)),
        '*' => Ok(Mul(op1, op2)),
        _ => Err(s.fail(&format!("Invalid operator {}", operator)))
    }
}

fn parse_targets(s: &mut Scanner) -> ParseResult<[usize; 2]> {
    let mut targets  = [0; 2];
    for _ in 0..2 {
        s.string("If ")?;
        let ix = s.string("true").and(Ok(1)).or_else(|_| s.string("false").and(Ok(0)))?;
        s.string(": throw to monkey ")?;
        let target = s.some(char::is_ascii_digit)?.parse().unwrap();
        targets[ix] = target;
        s.many(char::is_ascii_whitespace);
    }
    Ok(targets)
}

fn parse_monkey(s: &mut Scanner) -> ParseResult<Monkey> {
    s.string("Monkey ")?;
    s.skip_some(char::is_ascii_digit)?;
    s.next_is(':')?;
    s.skip_some(char::is_ascii_whitespace)?;

    s.string("Starting items: ")?;
    let items = parse_item_list(s)?;
    s.skip_some(char::is_ascii_whitespace)?;

    s.string("Operation: ")?;
    let operation = parse_operation(s)?;
    s.skip_some(char::is_ascii_whitespace)?;

    s.string("Test: ")?;
    let test = parse_test(s)?;
    s.skip_some(char::is_ascii_whitespace)?;
    let targets = parse_targets(s)?;

    Ok(Monkey {items, operation: operation.into(), test: Box::new(test), targets})
}

fn parse_monkeys(s: &mut Scanner) -> ParseResult<Vec<Monkey>> {
    let mut monkeys = Vec::new();
    while let Ok(monkey) = parse_monkey(s) {
        monkeys.push(monkey);
        s.skip_many(char::is_ascii_whitespace)?;
    }
    Ok(monkeys)
}

fn parse_input(s: &mut Scanner) -> ParseResult<Vec<Monkey>> {
    let result = parse_monkeys(s);
    s.eof()?;
    result
}

fn main() {
    let mut scanner = Scanner::lazy(std::io::stdin().lines().map(|l|l.unwrap()));
    let monkeys = parse_input(&mut scanner).unwrap();
}

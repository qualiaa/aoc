use std::collections::VecDeque;
use std::iter::empty;
use std::fmt;

// Disable Copy (hence implicit pass-by-value) to get compiler guarantees that
// moving moves.
struct Item {worry: usize}

use std::rc::Rc;
use std::cell::RefCell;

struct Test(usize);
impl Test {
    fn as_fn(self) -> impl FnMut(&Item) -> bool {
        move |i| i.worry % self.0 == 0
    }
}

struct Monkey {
    items: Vec<Item>,
    operation: Box<dyn FnMut(&mut Item)>,
    test: Box<dyn FnMut(&Item) -> bool>,
    targets: [Rc<RefCell<Monkey>>; 2]
}

impl Monkey {
    fn pass(&mut self, i: Item) {
        self.items.push(i)
    }

    fn update(&mut self) {
        for mut item in self.items.drain(..) {
            (self.operation)(&mut item);
            item.worry /= 3;
            self.targets[(self.test)(&item) as usize].borrow_mut().pass(item);
        }
    }
}

// A different tack for parsing: a scanner with a low memory footprint which can
// be lazy in its input. At some point I'll have another crack at parser
// combinators...
struct<SF> Scanner {
    string_feed: SF,
    // I considered using a Box<impl Iterator<Item=char>> here... still not sure
    // which I'd prefer. Would require maintaining another buffer for lookahead.
    // This way, we store slightly more in memory than we need and have to
    // maintain a local cursor, but otherwise it's quite clean.
    loaded: Vec<char>,
    cursor: usize,
    pos: ScanPos
}

struct ScanPos {
    chars: usize
    line: usize,
    col: usize
}

#[derive(Debug)]
impl ScanPos {
    fn new() -> Self {
        ScanPos {1, 1, 1}
    }

    fn proceed(&mut self, char c) {
        if c == '\n' {
            self.line += 1
            self.col = 1
        } else {
            self.col += 1
        }
        self.chars += 1
    }
}

impl fmt::Display for ScanPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.chars, self.line, self.col)
    }
}

#[derive(Debug)]
struct ParseError {
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

type ParseResult<T> = Result<T, ParseError>;

impl<SF> Scanner
    where SF: Iterator<String>
{
    fn strict(input: &str) -> Self {
        Scanner {
            empty(), input.chars().collect(), 0, ScanPos::new()
        }
    }

    fn lazy(input: SF) -> Self {
        Scanner{
            input, VecDeque::new(), 0, ScanPos::new()
        }
    }

    fn peek_by(&mut self, i: usize) -> ParseResult<char> {
        if let Some(c) = self.loaded.get(self.cursor + i) {
            Ok(c)
        } else if let Some(string) = self.string_feed.next() {
            self.loaded = self.loaded.split_off(self.cursor);
            self.loaded.extend(string.chars())
            self.cursor = 0;
            self.peek_by(i)
        } else {
            Err(self.fail(fmt!("input exhausted at offset {}", i)))
        }
    }

    fn fail<_>(&self, msg: &str) -> ParseError {
        ParseError { pos: self.pos, msg: String::from(msg), cause: None}
    }

    fn fail_from<_, E: Error>(&self, cause: E, msg: &str) -> ParseError {
        ParseError { cause: Some(Box::new(cause)), ..self.fail(msg) }
    }

    fn peek(&mut self) -> ParseResult<char> {
        self.peek_by(0)
    }

    fn next(&mut self) -> ParseResult<char> {
        self.peek().map(|c| {self.cursor += 1; self.pos.proceed(c); c })
    }

    fn eof(&mut self) -> ParseResult<()> {
        self.peek().and(
            Err(self.fail("EOL expected"))
        ).or(Ok(()))
    }

    fn skip(&mut self) -> ParseResult<()> {
        self.next().and(Ok(()))
    }

    fn satisfy(&mut self, p: FnOnce(char) -> bool) -> ParseResult<char> {
        match self.peek() {
            Ok(c) if p(c) => self.next(),
            Err(e) => Err(self.fail_from(e, "failed while matching predicate")),
            _ => Err(self.fail("predicate not satisfied"))
        }
    }

    fn next_is(&mut self, c: char) -> ParseResult<char> {
        self.satisfy(|x| x==c).map_err(
            |e| self.fail_from(e, format!("failed while seeking `{}'", c)))
    }

    fn string<'a>(&mut self, s: &str) -> ParseResult<String> {
        let cursor = self.cursor;
        for (i, c) in s.chars().enumerate() {
            match self.peek_by(i) {
                Ok(x) if c != x => Err(self.fail("could not match string `{}'", s))
                Err(e) => Err(self.fail_from(e, format!("failed while matching {}", s))),
                _ => ()
            }?
        }
        self.skip_by(s.len())
        Ok(String::from(s))
    }

    fn some(&mut self, p: FnMut(&char) -> bool) -> ParseResult<String> {
        let mut cs = Vec<char>;
        while let Ok(c) = self.peek() {
            if !p(c) {
                break
            }
            cs.append(self.next().unwrap());
        }
        if cs.empty() {
            self.peek().map_err(|e| self.fail_from(e, "while matching predicate"))
                .and(Err(self.fail("predicate could not match any chars")))
        } else {
            Ok(String::from_iter(cs))
        }
    }

    fn many(&mut self, p: FnMut(&char) -> bool) -> ParseResult<String> {
        self.some(p).or(Ok(String::new()))
    }

    fn skip_many(&mut self, p: FnMut(&char) -> bool) -> ParseResult<()> {
        self.many(p).and(Ok(()))
    }

    fn skip_some(&mut self, p: FnMut(&char) -> bool) -> ParseResult<()> {
        self.some(p).and(Ok(()))
    }
}

fn parse_item(s: &mut Scanner) -> ParseResult<Item> {
    Item {worry: s.some(char::is_ascii_digit)?.parse().unwrap()}
}

fn parse_item_list(s: &mut Scanner) -> ParseResult<Vec<Item>> {
    let mut items = vec![parse_item(s)?];

    while let Some(_) = s.string(", ") {
        items.append(parse_item(s)?)
    }
    Ok(items)
}

fn parse_test(s: &mut Scanner) -> ParseResult<impl FnMut(&Item) -> bool>
    s.string("divisible by ")?;
    let divisor = s.some(char::is_ascii_digit)?.parse<usize>().unwrap();
    Ok(move |item: Item| item.worry % divisor == 0)

enum Operand {
    Old,
    Num(usize)
}

fn parse_operand(s: &mut Scanner) -> ParseResult<Operand> {
    if let Ok(_) = s.string("old") {
        Ok(Operand::Old)
    } else if let Ok(digits) = s.some(char::is_ascii_digit) {
        Ok(Operand::Num(digits.parse().unwrap()))
    } else {
        Err(s.fail("no valid operand"))
    }
}

enum Operation {
    Add,
    Mul
}

impl Operation {
    fn as_fn(self) -> Box<dyn FnMut(&mut Item)> {
        match self {
            Add(x) => Box::new(move |i: &mut Item| i.worry += x),
            Mul(x) => Box::new(move |i: &mut Item| i.worry *= x)
        }
    }
}


fn parse_operation(s: &mut Scanner) -> ParseResult<impl FnMut(&mut Item)> {
    s.string("new = ")?;
    let op1 = parse_operand(s)?;
    // ...
}

fn parse_targets(s: &mut Scanner) -> ParseResult<[usize; 2]> {
    // ...
}

fn parse_monkey(s: &mut Scanner) -> ParseResult<Monkey> {
    s.string("Monkey ")?;
    s.skip_some(char::is_ascii_digit)?;
    s.is_next(':')?;
    s.skip_some(char::is_ascii_whitespace)?;

    s.string("Starting items: ")?;
    let items = parse_item_list(s)?;
    s.skip_some(char::is_ascii_whitespace)?;

    s.string("Operation: ")?;
    let operation = parse_operation(s)?;
    s.skip_some(char::is_ascii_whitespace)?;

    s.string("Test: ")?;
    let test = parse_test(s)?
    s.skip_some(char::is_ascii_whitespace)?;
    let targets = parse_targets()?;

    Monkey {items, operation.into(), test, targets}
}

fn parse_monkeys(s: &mut Scanner) -> ParseResult<Vec<Monkey>> {
    let mut monkeys = Vec::new();
    while let Ok(monkey) = parse_monkey(s) {
        monkeys.append(monkey);
        s.skip_many(char::is_ascii_whitespace)?;
    }
}

fn parse_input(s: &mut Scanner) -> ParseResult<Vec<Monkey>> {
    let result = parse_monkeys(s);
    s.eof()?
}

fn main() {
    let mut scanner = Scanner::lazy(std::io::stdin().lines());
    let monkeys = parse_input(&mut s).unwrap()
}

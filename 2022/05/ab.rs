use std::str::FromStr;
use std::io::Read;
// Had a go at implementing parser combinators similarly to how I'd do it in
// Haskell. Got surprisingly far, but difficulty getting the monad
// implementation to compile makes the rest of the implementation a bit gross.
// Probably all possible if I knew Rust.

mod pc {
    use std::convert::TryInto;
    pub type ParseErr = String;
    pub type ParseInput<'a> = &'a str;
    pub type ParseResult<'a, T> = Result<(T, &'a str), ParseErr>;

    pub trait Parse<'a, T> : FnMut(ParseInput<'a>) -> ParseResult<'a, T> {
        // Not sure how to make this to type/borrow check, or divide between
        // trait and impl...
        // This is annoying because all of the parser combinators would
        // be infinitely more pleasant to write with working map and bind
        // implementations.
        // It's possibly the case that I need to implement a more complicated
        // data structure than using functions/closures alone.
        // Or perhaps going through the pain of taking everything by reference
        // everywhere...

        //fn map<U, F, Ret>(&mut self, f: &mut F) -> Ret
        //    where
        //        F: FnMut(T) -> U,
        //        Ret: Parse<'a, U>;
        //{
        //    move |input| self(input).map(|(x, rest)| (f(x), rest))
        //}
        //fn and_then<U, Ret, F>(&mut self, f: &mut F) -> Ret
        //    where
        //        F: FnMut(T) -> Ret,
        //        Ret: Parse<'a, U>;
        //{
        //    move |input| self(input).and_then(|(x, rest)| f(x)(rest))
        //}

        //fn and_skip<U, P: Parse<'a, U>>(&mut self, p: &mut P) -> Self;
        //{
        //    self.and_then(|x| p.map(|_| x))
        //    //move |input| self(input).map(|(x, rest)| p.map(|_| x)(rest))
        //}

        //fn replace<'b, U, Ret: Parse<'a, U>>(&mut self, p: &'b mut Ret) -> &'b Ret;
        //{
        //    self.and_then(|_| p)
        //}
    }

    impl<'a, T, F> Parse<'a, T> for F where F: FnMut(ParseInput<'a>) -> ParseResult<'a, T> {
        //fn map<U, G, Ret>(&mut self, f: &mut G) -> Ret
        //    where
        //        G: FnMut(T) -> U,
        //        Ret: Parse<'a, U>
        //{
        //    |input| (&mut self)(input).map(|(x, rest)| ((&mut f)(x), rest))
        //}

        //fn and_then<U, Ret, G>(&mut self, g: &mut G) -> Ret
        //    where
        //        G: FnMut(T) -> Ret,
        //        Ret: Parse<'a, U>
        //{
        //    |input| (&mut self)(input).and_then(|(x, rest)| (&mut g)(x)(rest))
        //}

        //fn and_skip<U, P: Parse<'a, U>>(&mut self, p: &mut P) -> Self {
        //    self.and_then(|x| p.map(|_| x))
        //    //move |input| self(input).map(|(x, rest)| p.map(|_| x)(rest))
        //}

        //fn replace<'b, U, Ret: Parse<'a, U>>(&mut self, p: &'b mut Ret) -> &'b Ret {
        //    self.and_then(|_| p)
        //}
    }

    pub fn sep_by1<'a, T, X>(mut sep: impl Parse<'a, X>, mut p: impl Parse<'a, T>) -> impl Parse<'a, Vec<T>> {
        move |input| {
            p(input).map(|(first, rest)| {
                let mut values = vec![first];
                let mut rest: &str = rest;
                while let Ok((next, next_rest)) = sep(rest).and_then(|(_, rest)| p(rest)) {
                    values.push(next);
                    rest = next_rest;
                }
                (values, rest)
            })
        }
    }

    pub fn end_by1<'a, T, X>(mut term: impl Parse<'a, X>, mut p: impl Parse<'a, T>) -> impl Parse<'a, Vec<T>> {
        move |input| {
            let mut read_one = |x| (&mut p)(x).and_then(|(first, rest)| {
                (&mut term)(rest).map(|(_, rest)| (first, rest))
            });
            read_one(input).map(|(first, rest)| {
                let mut values = vec![first];
                let mut rest: &str = rest;
                while let Ok((next, next_rest)) = read_one(rest) {
                    values.push(next);
                    rest = next_rest;
                }
                (values, rest)
            })
        }
    }

    pub fn char_<'a>(c: char) -> impl Parse<'a, char> {
        move |input: ParseInput<'a>| {
            input.strip_prefix(c)
                .map(|rest| (c, rest))
                .ok_or(format!("Could not parse char: {}", c))
        }
    }

    pub fn string<'a>(s: &'a str) -> impl Parse<'a, &'a str> {
        move |input: ParseInput<'a>| {
            input.strip_prefix(s)
                .map(|rest| (s, rest))
                .ok_or(format!("Could not parse string: {}", s))
        }
    }

    pub fn satisfy<'a, P: FnMut(char) -> bool>(mut pred: P) -> impl Parse<'a, char> {
        move |input: ParseInput<'a>| {
            input.chars().next().and_then(|c| {
                if pred(c) {
                    Some((c, input.strip_prefix(c).unwrap()))
                } else {None}
            }).ok_or("Could not match predicate.".to_string())
        }
    }

    pub fn many<'a, T>(mut p: impl Parse<'a, T>) -> impl Parse<'a, Vec<T>> {
        move |input| {
            let mut rest = input;
            let mut data = Vec::new();
            while let Ok((next, next_rest)) = p(rest) {
                data.push(next);
                rest = next_rest;
            }
            Ok((data, rest))
        }
    }

    pub fn many1<'a, T>(mut p: impl Parse<'a, T>) -> impl Parse<'a, Vec<T>> {
        move |input| {
            p(input).map(|(first, mut rest)| {
                let mut data = vec![first];
                while let Ok((next, next_rest)) = p(rest) {
                    data.push(next);
                    rest = next_rest;
                }
                (data, rest)
            })
        }
    }
    // This falls foul of the borrow-checker due to a move of p making the return FnOnce
    //pub fn many1<'a, T>(mut p: impl Parse<'a, T>) -> impl Parse<'a, Vec<T>> {
    //    move |input| {
    //        p(input).map(|(first, rest)| {
    //            let (mut v, rest) = many(p)(rest).unwrap();
    //            let mut data = vec![first];
    //            data.append(&mut v);
    //            (data, rest)
    //        })
    //    }
    //}

    pub fn digits<'a>(input: ParseInput<'a>) -> ParseResult<'a, isize> {
        let mut digits = many1(satisfy(|c| c.is_ascii_digit()));
        fn do_parse(v: Vec<char>) -> isize {
            v.into_iter().collect::<String>().parse().unwrap()
        }
        input.strip_prefix('-').ok_or("".to_string())
            .and_then(|rest| digits(rest).map(|(x, rest)| (-do_parse(x), rest)))
            .or_else(|_| digits(input).map(|(x, rest)| (do_parse(x), rest)))
    }

    pub fn count<'a, T>(n: usize, mut p: impl Parse<'a, T>) -> impl Parse<'a, Vec<T>> {
        move |input| {
            let mut arr: Vec<T> = Vec::new();
            let mut rest = input;
            for _ in 0..n {
                let (next, next_rest) = p(rest)?;
                arr.push(next);
                rest = next_rest;
            }
            Ok((arr, rest))
        }
    }

    pub fn skip_until<'a, T>(mut term: impl Parse<'a, T>) -> impl Parse<'a, T> {
        move |input: ParseInput<'a>| {
            let mut rest = input;
            while !rest.is_empty() {
                match term(rest) {
                    result @ Ok(_) => return result,
                    Err(_) => rest = &rest[1..]
                }
            }
            Err("skip_until: input exhausted.".to_string())
        }
    }
}

use pc::{ParseInput, ParseResult};

#[derive(Debug, Clone)]
struct Crate(char);
impl FromStr for Crate {
    type Err = pc::ParseErr;

    fn from_str(s: &str) -> Result<Crate, Self::Err> {
        let chars: Vec<char> = s.chars().take(3).collect();
        match chars[..] {
            ['[', c, ']'] => Ok(Crate(c)),
            _ => Err(String::from("Could not parse crate: ") + &s[..3])
        }
    }
}

fn parse_crate<'a>(input: ParseInput<'a>) -> ParseResult<'a, Crate> {
    if input.len() < 3 {
        return Err(String::from("Past end of string"));
    }
    let (chunk, rest) = input.split_at(3);
    chunk.parse::<Crate>().map(|c| (c, rest))
}

fn parse_crate_maybe<'a>(input: ParseInput<'a>) -> ParseResult<'a, Option<Crate>> {
    parse_crate(input).map(|(c, rest)| (Some(c), rest))
        .or_else(|_| pc::string("   ")(input).map(|(_, rest)| (None, rest)))
}

#[derive(Debug)]
struct Move {
    n: usize,
    from: usize,
    to: usize
}

fn parse_move<'a>(input: ParseInput<'a>) -> ParseResult<'a, Move> {
    pc::string("move ")(input).and_then(|(_, rest)| {
        pc::digits(rest).and_then(|(n, rest)| {
            pc::string(" from ")(rest).and_then(|(_, rest)| {
                pc::digits(rest).and_then(|(from, rest)| {
                    pc::string(" to ")(rest).and_then(|(_, rest)| {
                        pc::digits(rest).map(|(to, rest)| {
                            (Move {n: n as usize,
                                   from: from as usize,
                                   to: to as usize},
                             rest)
                        })
                    })
                })
            }) // weee
        })
    })
}

fn parse_input<'a>(input: ParseInput<'a>) -> ParseResult<'a, (Vec<Vec<Crate>>, Vec<Move>)> {
    let row = pc::sep_by1(pc::char_(' '), parse_crate_maybe);
    let mut crates = pc::end_by1(pc::char_('\n'), row);
    let mut moves = pc::sep_by1(pc::char_('\n'), parse_move);
    let mut junk = pc::count(2, pc::skip_until(pc::char_('\n')));
    crates(input).and_then(|(crates, rest)| {
        junk(rest).and_then(|(_, rest)| {
            moves(rest).map(|(moves, rest)| {
                ((columns(crates), moves), rest)
            })
        })
    })
}

fn columns(rows: Vec<Vec<Option<Crate>>>) -> Vec<Vec<Crate>> {
    rows.iter().map(|row| row.len()).max().map(|n_cols| {
        let mut result = vec![Vec::new(); n_cols];
        for row in rows.into_iter().rev() {
            for (i, element) in row.into_iter().enumerate() {
                element.map(|c| result[i].push(c));
            }
        }
        result
    }).unwrap_or(Vec::new())
}

fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input);

    let (crates, moves) = parse_input(&input).unwrap().0;

    let (mut a, mut b) = (crates.clone(), crates.clone());
    for Move {n, from, to} in moves {
        let len = b[from-1].len();
        let mut cs: Vec<Crate> = b[from-1].drain(len-n..).collect();
        b[to-1].append(&mut cs);
        for _ in 0..n {
            let c = a[from-1].pop().unwrap();
            a[to-1].push(c);
        }
    }

    fn tops(crates: Vec<Vec<Crate>>) -> impl Iterator<Item=char> {
        crates.into_iter().map(|col| col.last().unwrap().0)
    }
    let a: String = tops(a).collect();
    let b: String = tops(b).collect();

    println!("{}\n{}", a, b);
}

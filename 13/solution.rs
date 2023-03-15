use std::cmp;
use std::fs;
use std::iter::Peekable;
use std::str::{Chars, FromStr};
use std::string::ParseError;

#[derive(Debug, Clone)]
enum Packet {
    One(i32),
    Many(Vec<Packet>),
}

fn parse_one(s: &mut Peekable<Chars<'_>>) -> Packet {
    let mut sum = 0;
    while let Some(c) = s.next_if(|c| c.is_digit(10)) {
        sum = sum * 10 + (c as i32 - 48);
    }
    Packet::One(sum)
}

fn parse_many(s: &mut Peekable<Chars<'_>>) -> Packet {
    let mut packets = Vec::new();
    s.next();
    loop {
        match s.peek() {
            Some(',') => {
                s.next();
            }
            Some(']') => {
                s.next();
                return Packet::Many(packets);
            }
            Some(_) => packets.push(parse_packet(s)),
            None => unreachable!(),
        }
    }
}

fn parse_packet(s: &mut Peekable<Chars<'_>>) -> Packet {
    match s.peek() {
        Some('[') => parse_many(s),
        Some(_) => parse_one(s),
        None => unreachable!(),
    }
}

impl FromStr for Packet {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(parse_many(&mut s.chars().peekable()))
    }
}

impl cmp::PartialEq for Packet {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Packet::One(x), Packet::One(y)) => x == y,
            (Packet::One(x), Packet::Many(ys)) => vec![Packet::One(*x)] == *ys,
            (Packet::Many(xs), Packet::One(y)) => *xs == vec![Packet::One(*y)],
            (Packet::Many(xs), Packet::Many(ys)) => xs == ys,
        }
    }
}

impl cmp::Eq for Packet {}

impl cmp::Ord for Packet {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        match (self, other) {
            (Packet::One(x), Packet::One(y)) => x.cmp(y),
            (Packet::One(x), Packet::Many(ys)) => vec![Packet::One(*x)].cmp(ys),
            (Packet::Many(xs), Packet::One(y)) => xs.cmp(&vec![Packet::One(*y)]),
            (Packet::Many(xs), Packet::Many(ys)) => xs.cmp(ys),
        }
    }
}

impl cmp::PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn main() {
    let input = fs::read_to_string("./input").expect("whatever");

    let mut packets = input
        .lines()
        .filter(|l| l.len() > 0)
        .map(|p| p.parse::<Packet>().unwrap())
        .collect::<Vec<Packet>>();

    let part1 = packets
        .chunks(2)
        .enumerate()
        .fold(0, |sum, (i, chunk)| match chunk {
            [l, r] => sum + if l <= r { i + 1 } else { 0 },
            _ => unreachable!(),
        });

    let part2 = {
        let signals = ["[[2]]", "[[6]]"].map(|p| p.parse::<Packet>().unwrap());
        let signals2 = signals.clone();
        for s in signals {
            packets.push(s);
        }
        packets.sort();

        signals2.iter().fold(1, |prod, p| {
            prod * (packets.iter().position(|p1| p1 == p).unwrap() + 1)
        })
    };

    println!("part 1: {}, part 2: {}", part1, part2)
}

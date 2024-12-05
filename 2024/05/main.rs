use std::cmp;
use std::collections::{HashMap, HashSet};
use std::fs;

fn parse_int(input: &str) -> i32 {
    input.parse::<i32>().unwrap()
}

fn parse(input: &str) -> (HashMap<i32, HashSet<i32>>, Vec<Vec<i32>>) {
    let (rules_str, pages_str) = input.trim().split_once("\n\n").unwrap();

    let rules = rules_str
        .lines()
        .map(|line| line.split_once('|').unwrap())
        .fold(
            HashMap::new(),
            |mut map: HashMap<i32, HashSet<i32>>, (before, after)| {
                let list = map.entry(parse_int(before)).or_insert(HashSet::new());
                list.insert(parse_int(after));
                map
            },
        );

    let pages: Vec<Vec<i32>> = pages_str
        .lines()
        .map(|line| line.split(',').map(parse_int).collect())
        .collect();

    (rules, pages)
}

fn solve(input: &str) -> (i32, i32) {
    let (rules, pages) = parse(input);

    let answers: Vec<_> = pages
        .iter()
        .map(|page| {
            let mut sorted = page.clone();
            let empty = HashSet::new();
            sorted.sort_by(|a, b| {
                let ra = rules.get(a).unwrap_or(&empty);
                let rb = rules.get(b).unwrap_or(&empty);
                if ra.contains(b) {
                    cmp::Ordering::Less
                } else if rb.contains(a) {
                    cmp::Ordering::Greater
                } else {
                    cmp::Ordering::Equal
                }
            });

            if sorted != *page {
                (0, sorted[page.len() / 2])
            } else {
                (page[page.len() / 2], 0)
            }
        })
        .collect();

    let a1 = answers.iter().map(|x| x.0).sum();
    let a2 = answers.iter().map(|x| x.1).sum();

    (a1, a2)
}

fn main() {
    let i0 = fs::read_to_string("./05/input0").unwrap();
    println!("{:?}", solve(&i0));

    let i1 = fs::read_to_string("./05/input1").unwrap();
    println!("{:?}", solve(&i1));

    let i = fs::read_to_string("./05/input").unwrap();
    println!("{:?}", solve(&i));
}

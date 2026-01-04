use std::collections::HashSet;

use aoc2026::Inputs;

fn main() {
    let inputs = Inputs::read();

    solve(&inputs.test_inputs[0]);

    println!("==========");
    solve(&inputs.input);
}

fn solve(input: &str) {
    let ranges = input.trim().split(",").map(|r| {
        let (s, e) = r.split_once("-").unwrap();
        (s.parse::<i64>().unwrap(), e.parse::<i64>().unwrap())
    }).collect::<Vec<_>>();

    let mut p1 = 0;
    let mut p2 = 0;
    for r in ranges.iter() {
        p1 += count_invalid(r).iter().sum::<i64>();
        p2 += count_invalid2(r) .iter().sum::<i64>();
    }

    println!("part1: {} part2: {}", p1, p2);
}

fn count_invalid(range: &(i64, i64)) -> HashSet<i64> {
    let mut invalid = HashSet::<i64>::new();
    let mut pattern = half(range.0);
    let mut id = append(pattern, pattern);
    
    while id <= range.1 {
        if range.0 <= id {
            invalid.insert(id);
        }

        pattern += 1;
        id = append(pattern, pattern);
    }

    invalid
}

fn count_invalid2(range: &(i64, i64)) -> HashSet<i64> {
    let end_str = range.1.to_string();
    let end = end_str[0..end_str.len() / 2 + 1].parse::<i64>().unwrap();
    let mut invalid = HashSet::<i64>::new();

    for pattern in 1..end {
        let mut id = append(pattern, pattern);
        
        while id <= range.1 {
            if range.0 <= id {
                invalid.insert(id);
            }

            id = append(id, pattern);
        }
    }

    invalid
}

fn append(x: i64, y: i64) -> i64 {
    format!("{}{}", x, y).parse::<i64>().unwrap()
}

fn half(x: i64) -> i64 {
    let s = x.to_string();
    if s.len() <= 1 {
        return 1
    }

    let l = &s[0..s.len() / 2];

    l.parse::<i64>().unwrap()
}

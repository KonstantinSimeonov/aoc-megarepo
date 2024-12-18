use regex::Regex;
use std::fs;

#[derive(PartialEq, Eq, Debug)]
enum Cmd {
    Mul(i32, i32),
    Do,
    Dont,
}

fn solve(input: &str) -> (i32, i32) {
    let stuff = Regex::new(r"mul\((\d+),(\d+)\)|do\(\)|don't\(\)").unwrap();
    let commands = stuff.captures_iter(input).map(|caps| {
        let captures = caps
            .iter()
            .filter_map(|xs| xs.map(|m| m.as_str()))
            .collect::<Vec<_>>();

        match captures[..] {
            ["do()"] => Cmd::Do,
            ["don't()"] => Cmd::Dont,
            [_, x, y] => Cmd::Mul(x.parse::<i32>().unwrap(), y.parse::<i32>().unwrap()),
            _ => panic!(),
        }
    });

    let mut a1 = 0;
    let mut a2 = 0;
    let mut enabled = true;

    for cmd in commands {
        if let Cmd::Mul(x, y) = cmd {
            a1 += x * y;
            if enabled {
                a2 += x * y;
            }
        } else {
            enabled = cmd == Cmd::Do;
        }
    }

    (a1, a2)
}

fn main() {
    let tests = ["./03/input0", "./03/input1", "./03/input"];
    for t in tests {
        let input = fs::read_to_string(t).unwrap();
        println!("{} {:?}", t, solve(&input));
    }
}

use core::fmt;
use std::fs;

fn int(input: &str) -> i64 {
    input.parse::<i64>().unwrap()
}

fn parse_line(input: &str) -> (i64, Vec<i64>) {
    let (target, rest) = input.split_once(": ").unwrap();
    let args: Vec<i64> = rest.split(" ").map(int).collect();

    return (int(&target), args);
}

fn cat(x: i64, y: i64) -> i64 {
    int(&format!("{}{}", x, y))
}

fn add(x: i64, y: i64) -> i64 {
    x + y
}

fn mul(x: i64, y: i64) -> i64 {
    x * y
}

fn can_do(target: i64, ops: &[fn(i64, i64) -> i64], args: &[i64], current: i64) -> bool {
    if target < current {
        return false;
    }

    if args.is_empty() {
        return target == current;
    }

    ops.iter()
        .any(|op| can_do(target, ops, &args[1..], op(current, args[0])))
}

fn solve(input: &str) -> (i64, i64) {
    let mut a1 = 0;
    let mut a2 = 0;

    for line in input.trim().lines() {
        let (target, args) = parse_line(line);
        if can_do(target, &[mul, add], &args[1..], args[0]) {
            a1 += target;
            a2 += target;
        } else if can_do(target, &[cat, mul, add], &args[1..], args[0]) {
            a2 += target;
        }
    }

    (a1, a2)
}

fn main() {
    let i0 = fs::read_to_string("./07/input0").unwrap();
    println!("{:?}", solve(&i0));

    let i = fs::read_to_string("./07/input").unwrap();
    println!("{:?}", solve(&i));
}

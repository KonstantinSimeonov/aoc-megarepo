use std::fs;

fn byte_to_012(x: u8) -> u32 {
    ((x as u32) - 65) % 23
}

fn prev(x: u32) -> u32 {
    (5 - x * 2) % 3
}

fn next(x: u32) -> u32 {
    (x + 1) % 3
}

fn main() {
    let input = fs::read_to_string("./input").expect("stuff");

    let turns = input
        .trim()
        .lines()
        .map(|line| {
            (
                byte_to_012(line.bytes().next().unwrap()),
                byte_to_012(line.bytes().last().unwrap()),
            )
        })
        .collect::<Vec<(u32, u32)>>();

    let part2: u32 = turns
        .iter()
        .map(|(ar, br)| {
            let a = *ar;
            match *br {
                0 => prev(a) + 1,
                1 => a + 3 + 1,
                2 => next(a) + 6 + 1,
                _ => 0,
            }
        })
        .sum();

    let part1: u32 = turns
        .into_iter()
        .map(|(a, b)| {
            let bonus = if a == b {
                3
            } else if next(a) == b {
                6
            } else {
                0
            };

            b + 1 + bonus
        })
        .sum();

    println!("{} {}", part1, part2)
}

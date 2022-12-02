use std::fs;

fn byte_to_012(x: u8) -> u32 {
    ((x as u32) - 65) % 23
}

fn main() {
    let input = fs::read_to_string("./input")
        .expect("stuff");

    let turns = input
        .trim()
        .lines()
        .map(|line| (
            byte_to_012(line.bytes().next().unwrap()),
            byte_to_012(line.bytes().last().unwrap())
        ))
        .collect::<Vec<(u32, u32)>>();

    let part2: u32 = turns
        .iter()
        .map(|(ar, br)| {
            let a = *ar;
            match *br {
                0 if a == 0 => 3,
                0 => a,
                1 => a + 3 + 1,
                2 => ((a + 1) % 3) + 6 + 1,
                _ => 0
            }
        })
        .sum();

    let part1: u32 = turns
        .into_iter()
        .map(|(a, b)| {
            let bonus =
                if a == b { 3 }
                else if (a + 1) % 3 == b { 6 }
                else { 0 };

            b + 1 + bonus
        })
        .sum();



    println!("{} {}", part1, part2)
}

use std::fs;

fn main() {
    let input = fs::read_to_string("./input")
        .expect("To read the file")
        .lines()
        .map(|x| x.parse::<u16>().unwrap())
        .collect::<Vec<u16>>();

    let mut part1: u16 = 0;
    let mut part2: u16 = 0;
    for i in 1..input.len() {
        part1 += (input[i - 1] < input[i]) as u16;
        if i < input.len() - 2 {
            part2 += (input[i - 1] < input[i + 2]) as u16;
        }
    }

    println!("part 1: {}, part 2: {}", part1, part2)
}

use std::fs;
use std::io::*;

fn solve_stupid() {
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

    println!("part 1: {}, part 2: {}", part1, part2);
}

fn solve_not_so_stupid() {
    let reader = BufReader::new(fs::File::open("./input").unwrap())
        .lines()
        .map(|l| l.expect("To be a number line").parse::<u16>().unwrap());

    let mut xs: Vec<u16> = vec![9000, 9000, 9000, 9000];
    let mut part1: u16 = 0;
    let mut part2: u16 = 0;
    for m in reader {
        xs[0] = xs[1];
        xs[1] = xs[2];
        xs[2] = xs[3];
        xs[3] = m;

        part1 += (xs[0] < xs[1]) as u16;
        part2 += (xs[0] < xs[3]) as u16;
    }

    part1 += (xs[1] < xs[2]) as u16 + (xs[2] < xs[3]) as u16;

    println!("part 1: {}, part 2: {}", part1, part2);
}

fn main() {
    solve_stupid();
    solve_not_so_stupid();
}

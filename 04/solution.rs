use std::fs;
use std::iter::Sum;

struct U32Pair(u32, u32);

// for the lolz
impl Sum for U32Pair {
    fn sum<I: Iterator<Item=Self>>(iter: I) -> Self {
        iter.fold(U32Pair(0, 0), |U32Pair(f, s), U32Pair(x, y)| U32Pair(f + x, s + y))
    }
}

fn main() {
    let input = fs::read_to_string("./input")
        .expect("ranges");

    let U32Pair(part1, part2) = input
        .lines()
        .map(|line| {
            let ranges = line
                .split(|c| c == '-' || c == ',')
                .map(|d| d.parse::<u32>().unwrap())
                .collect::<Vec<u32>>();

            match ranges[..] {
                [a, b, x, y] => U32Pair(
                    ((x <= a && b <= y) || (a <= x && y <= b)) as u32,
                    (
                        (x <= a && a <= y)
                        || (x <= b && b <= y)
                        || (a <= x && x <= b)
                        || (a <= y && y <= b)
                    ) as u32
                ),
                _ => U32Pair(0, 0)
            }
        })
        .sum();
        //.fold((0, 0), |(p1, p2), (a, b)| (p1 + a, p2 + b));

    println!("{} {}", part1, part2)
}

use std::fs;

fn main() {
    let input = fs::read_to_string("./input")
        .expect("To read the file");

    let length = 12;

    let gamma = input
        .lines()
        .fold(
            vec![0; length],
            |mut mem, str| {
                for (i, c) in str.chars().enumerate() {
                    mem[i] += ((c == '1') as i32 * 2) - 1;
                }
                return mem
            }
        )
        .into_iter()
        .fold(0, |gamma, bit| gamma * 2 + (bit > 0) as i32);

    let epsilon = gamma ^ ((1 << length) - 1);

    println!("g: {} e: {}, part 1: {}", gamma, epsilon, gamma * epsilon);
}

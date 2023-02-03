use std::fs;

fn main() {
    let input = fs::read_to_string("./input").expect("stuff");

    let max3: Vec<u32> = input
        .trim()
        .split("\n\n")
        .map(|line| line.split('\n').map(|c| c.parse::<u32>().unwrap()).sum())
        .fold(vec![0, 0, 0], |mut v, c| {
            v.push(c);
            v.sort_by(|a, b| b.cmp(a));
            v.pop();
            v
        });

    let max = max3[0];
    let all: u32 = max3.iter().sum();

    println!("part 1: {}, part 2: {:?}", max, all)
}

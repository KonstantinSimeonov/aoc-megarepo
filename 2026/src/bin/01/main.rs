use aoc2026::Inputs;

fn main() {
    let inputs = Inputs::read();
    solve(inputs.input);
}

fn solve(input: String) {
    let (_, part1, part2) = input
        .split("\n")
        .map(|line| {
            line.replace("R", "")
                .replace("L", "-")
                .parse::<i32>()
                .unwrap()
        })
        .fold((50, 0, 0), |(from, ended_on_zero, total_clicks), by| {
            let (next, clicks) = rotate(from, by);
            (next, ended_on_zero + if next == 0 { 1 } else { 0 }, total_clicks + clicks)
        });

    println!("part1: {}, part2: {}", part1, part2);
}

fn rotate(from: i32, by: i32) -> (i32, i32) {
    let next_unconstrained = from + (by % 100);
    let mut clicks = by.abs() / 100;

    if next_unconstrained >= 100 || (from != 0 && next_unconstrained <= 0) {
        clicks += 1;
    }

    let next = (next_unconstrained + 100) % 100;

    (next, clicks)
}

use aoc2026::Inputs;

fn main() {
    let inputs = Inputs::read();
    solve(&inputs.test_inputs[0]);
    solve(&inputs.test_inputs[1]);
    solve(&inputs.input);
}

fn solve(input: &str) {
    let (range_lines, id_lines) = input.split_once("\n\n").unwrap();
    let mut ranges: Vec<(u64, u64)> = range_lines
        .lines()
        .map(|range| {
            let (s, e) = range.split_once("-").unwrap();
            (s.parse::<u64>().unwrap(), e.parse::<u64>().unwrap())
        })
        .collect();
    let ids: Vec<u64> = id_lines
        .lines()
        .map(|id| id.parse::<u64>().unwrap())
        .collect();

    let fresh: usize = ids
        .iter()
        .filter(|&id| ranges.iter().any(|(s, e)| s <= id && id <= e))
        .count();

    println!("part 1: {}", fresh);

    // sort the ranges so that start(n) is always < start(n + 1)
    ranges.sort();

    let mut joined_ranges: Vec<(u64, u64)> = vec![ranges[0]];

    for i in 1..ranges.len() {
        let (s, e) = ranges[i];
        let mut joined = false;
        for j in 0..joined_ranges.len() {
            // s - 1 to handle [x, n] <-> [n + 1, y]
            if joined_ranges[j].1 >= s - 1 {
                // max to handle cases like [1, 10] <-> [3, 4]
                joined_ranges[j].1 = e.max(joined_ranges[j].1);
                joined = true;
                break;
            }
        }

        if !joined {
            joined_ranges.push((s, e))
        }
    }

    // + 1 to include range end
    let part2: u64 = joined_ranges.iter().map(|(s, e)| e - s + 1).sum();

    println!("part 2: {}", part2);
    println!("======");
}

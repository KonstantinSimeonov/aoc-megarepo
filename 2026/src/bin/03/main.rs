use aoc2026::Inputs;

fn main() {
    let inputs = Inputs::read();
    solve(&inputs.test_inputs[0]);
    println!("=========");
    solve(&inputs.input);
}

fn solve(input: &str) {
    let mut p1 = 0;
    let mut p2 = 0;

    for line in input.split("\n") {
        let bats = line.chars().map(|x| x as i64 - 48).collect::<Vec<_>>();
        let max_jolt_2 = find_max_jolt_2(&bats);
        let max_jolt_12 = find_max_jolt(&bats, 12);

        println!("{} {} {}", line, max_jolt_2, max_jolt_12);

        p1 += max_jolt_2;
        p2 += max_jolt_12;
    }

    println!("part1: {} part2: {}", p1, p2);
}

fn find_max_jolt_2(xs: &Vec<i64>) -> i64 {
    let mut f = 0;
    let mut s = 0;

    for i in 0..xs.len() {
        if xs[i] > f && i != xs.len() - 1 {
            f = xs[i];
            s = 0;
        } else if xs[i] > s {
            s = xs[i];
        }
    }

    f * 10 + s
}

fn find_max_jolt(xs: &Vec<i64>, digits: i64) -> i64 {
    let mut sorted = xs.iter().zip(0..).collect::<Vec<(&i64, i64)>>();
    // by digit desc first and by position asc second
    sorted.sort_by(|a, b| match b.0.cmp(a.0) {
        std::cmp::Ordering::Equal => a.1.cmp(&b.1),
        x => x,
    });

    let len = xs.len() as i64;
    let mut current_start = -1;
    let mut digits_left = digits;
    let mut jolt = 0;

    while digits_left > 0 {
        let (digit, index) = sorted
            .iter()
            .find(|(_, index)| current_start < *index && index + digits_left <= len)
            .unwrap();

        jolt = jolt * 10 + *digit;
        current_start = *index;
        digits_left -= 1;
    }

    jolt
}

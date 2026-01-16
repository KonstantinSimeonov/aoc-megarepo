use aoc2026::Inputs;

fn main() {
    let inputs = Inputs::read();
    solve(&inputs.test_inputs[0]);
    //solve(&inputs.test_inputs[1]);
    solve(&inputs.input);
}

fn solve(input: &str) {
    let grid: Vec<Vec<&str>> = input
        .lines()
        .map(|line| line.split_whitespace().collect())
        .collect();

    let problems = col_to_row(grid);

    let part1: u64 = problems
        .iter()
        .map(|problem| -> u64 {
            let nums_iter = problem
                .iter()
                .take(problem.len() - 1)
                .map(|n| n.parse::<u64>().unwrap());

            if *problem.last().unwrap() == "*" {
                nums_iter.product()
            } else {
                nums_iter.sum()
            }
        })
        .sum();

    let column_lens: Vec<usize> = problems
        .iter()
        .map(|row| {
            row.iter()
                .map(|&num| num.len())
                .max()
                .unwrap()
        })
        .collect();

    let grid_with_spaces: Vec<Vec<&str>> = input
        .lines()
        .map(|line| {
            let mut i = 0;
            let mut row: Vec<&str> = vec![];
            for len in column_lens.iter() {
                row.push(&line[i..(i + len).min(line.len())]);
                i += len + 1;
            }

            row
        })
        .collect();

    let problems_with_spaces = col_to_row(grid_with_spaces);
    let part2: u64 = problems_with_spaces.iter().map(|problem| -> u64 {
        let nums_iter = (0..problem[0].len())
            .map(|i| {
                problem.iter().take(problem.len() - 1).fold(0, |m, n| {
                    let s = &n[i..i + 1];
                    if s == " " {
                        m
                    } else {
                        m * 10 + s.parse::<u64>().unwrap()
                    }
                })
            });

        if problem.last().unwrap().trim() == "*" {
            nums_iter.product()
        } else {
            nums_iter.sum()
        }
    }).sum();

    println!("part 1: {} part 2: {}", part1, part2);
}

fn col_to_row(grid: Vec<Vec<&str>>) -> Vec<Vec<&str>> {
    let mut result: Vec<Vec<&str>> = Vec::with_capacity(grid[0].len());

    for c in 0..grid[0].len() {
        let mut row = vec![];
        for r in 0..grid.len() {
            row.push(grid[r][c]);
        }

        result.push(row);
    }

    result
}

//The rightmost problem is 4 + 431 + 623 = 1058
//The second problem from the right is 175 * 581 * 32 = 3253600
//The third problem from the right is 8 + 248 + 369 = 625
//Finally, the leftmost problem is 356 * 24 * 1 = 8544

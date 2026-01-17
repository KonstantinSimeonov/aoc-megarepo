use aoc2026::Inputs;

fn main() {
    let inputs = Inputs::read();
    solve(&inputs.test_inputs[0], true);
    solve(&inputs.input, false);
}

fn solve(input: &str, debug: bool) {
    let mut grid: Vec<Vec<i64>> = input
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| match c {
                    '^' => -1,
                    'S' => 1,
                    '.' => 0,
                    _ => panic!(),
                })
                .collect()
        })
        .collect();

    let mut splits = 0;

    for r in 0..grid.len() - 1 {
        for c in 0..grid[r].len() {
            let this = grid[r][c];
            let down = grid[r + 1][c];

            if this <= 0 {
                continue;
            }

            if down < 0 {
                if 0 < c {
                    grid[r + 1][c - 1] += this;
                }

                if c < grid[r].len() - 1 {
                    grid[r + 1][c + 1] += this;
                }

                if 0 < c || c < grid[r].len() - 1 {
                    splits += 1;
                }
            } else {
                grid[r + 1][c] += this;
            }
        }
    }

    if debug {
        render(&grid);
    }

    let timelines: i64 = grid.last().unwrap().iter().sum();

    println!("part 1: {} part 2: {}", splits, timelines);
}

fn render(grid: &Vec<Vec<i64>>) {
    for row in grid.iter() {
        for &x in row.iter() {
            let color = match x {
                -1 => "\x1b[33m",
                0 => "\x1b[0m",
                _ => "\x1b[32m",
            };

            print!("{}{:>4}{}", color, x, "\x1b[0m")
        }

        println!()
    }
}

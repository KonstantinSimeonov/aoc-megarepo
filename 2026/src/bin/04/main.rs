use aoc2026::Inputs;

fn main() {
    let inputs = Inputs::read();
    solve(&inputs.test_inputs[0]);
    solve(&inputs.input);
}

fn solve(lines: &str) {
    let mut input: Vec<Vec<bool>> = lines
        .lines()
        .map(|line| line.chars().map(|c| c == '@').collect())
        .collect();

    let (rows, cols) = (input.len(), input[0].len());

    let mut part1 = 0;
    for r in 0..rows {
        for c in 0..cols {
            if can_remove_roll(&input, r, c) {
                part1 += 1;
            }
        }
    }

    let mut part2 = 0;
    for r in 0..rows {
        for c in 0..cols {
            if can_remove_roll(&input, r, c) {
                part2 += remove_rolls(&mut input, r, c);
            }
        }
    }

    println!("part 1: {}, part2: {}", part1, part2);
}

fn can_remove_roll(input: &[Vec<bool>], r: usize, c: usize) -> bool {
    input[r][c] && neighbours_count(input, r, c) < 4
}

fn remove_rolls(input: &mut [Vec<bool>], r: usize, c: usize) -> usize {
    input[r][c] = false;
    let mut removed = 1;
    for (nr, nc) in neighbours(input.len(), input[0].len(), r, c) {
        if can_remove_roll(input, nr, nc) {
            removed += remove_rolls(input, nr, nc);
        }
    }

    removed
}

fn neighbours_count(input: &[Vec<bool>], r: usize, c: usize) -> usize {
    neighbours(input.len(), input[0].len(), r, c)
        .filter(|&(r, c)| input[r][c])
        .count()
}

fn neighbours(
    rows: usize,
    cols: usize,
    r: usize,
    c: usize,
) -> impl Iterator<Item = (usize, usize)> {
    DELTAS.iter().filter_map(move |&(dr, dc)| {
        let nr = r.checked_add_signed(dr)?;
        let nc = c.checked_add_signed(dc)?;
        (nr < rows && nc < cols).then_some((nr, nc))
    })
}

static DELTAS: [(isize, isize); 8] = [
    (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1),
];

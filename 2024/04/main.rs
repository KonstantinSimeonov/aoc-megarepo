use std::fs;

fn get(rows: &Vec<Vec<char>>, r: i32, c: i32) -> Option<&char> {
    rows.get(r as usize).and_then(|row| row.get(c as usize))
}

fn is_ms(a: Option<&char>, b: Option<&char>) -> usize {
    match (a, b) {
        (Some('M'), Some('S')) => 1,
        (Some('S'), Some('M')) => 1,
        _ => 0,
    }
}

fn search(rows: &Vec<Vec<char>>, r: i32, c: i32) -> usize {
    let deltas = (-1..=1)
        .flat_map(|dr| (-1..=1).map(move |dc| (dr, dc)))
        .filter(|&delta| delta != (0, 0));

    deltas
        .filter(|(dr, dc)| {
            let string = (1..4)
                .filter_map(|i| get(rows, r + dr * i, c + dc * i))
                .collect::<String>();

            string == "MAS"
        })
        .count()
}

fn parse(input: &str) -> Vec<Vec<char>> {
    input
        .split('\n')
        .map(|line| line.chars().collect())
        .collect()
}

fn solve(input: &str) -> (usize, usize) {
    let rows: Vec<Vec<char>> = parse(&input);

    let mut a1 = 0;
    let mut a2 = 0;

    for r in 0..rows.len() {
        for c in 0..rows[r].len() {
            if rows[r][c] == 'X' {
                a1 += search(&rows, r as i32, c as i32);
            }

            if rows[r][c] == 'A' {
                let ri = r as i32;
                let ci = c as i32;

                a2 += is_ms(get(&rows, ri - 1, ci - 1), get(&rows, ri + 1, ci + 1))
                    * is_ms(get(&rows, ri - 1, ci + 1), get(&rows, ri + 1, ci - 1));
            }
        }
    }

    (a1, a2)
}

fn main() {
    let input0 = fs::read_to_string("./04/input0").unwrap();
    println!("{:?}", solve(&input0));

    let input = fs::read_to_string("./04/input").unwrap();
    println!("{:?}", solve(&input));
}

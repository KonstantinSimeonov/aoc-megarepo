use std::{collections::HashSet, fs, thread};

type Pos = (usize, usize);

fn parse(input: &str) -> (Pos, Vec<Vec<char>>) {
    let mut start: Pos = (0, 0);

    let field: Vec<Vec<char>> = input
        .trim()
        .lines()
        .zip(0..)
        .map(|(line, row)| {
            if let Some(col) = line.find("^") {
                start = (row, col);
            }

            line.chars().collect()
        })
        .collect();

    (start, field)
}

fn move_coord(x: usize, y: i32, bound: usize) -> Option<usize> {
    let res = x as i32 + y;

    if 0 <= res && res < (bound as i32) {
        Some(res as usize)
    } else {
        None
    }
}

fn move_pos(pos: &Pos, dir: &(i32, i32), bounds: &(usize, usize)) -> Option<Pos> {
    let mr = move_coord(pos.0, dir.0, bounds.0)?;
    let mc = move_coord(pos.1, dir.1, bounds.1)?;
    Some((mr, mc))
}

fn walk(start: &Pos, field: &mut Vec<Vec<char>>) -> bool {
    let mut pos = start.clone();
    let mut dir = (-1, 0);
    let bounds = (field.len(), field[0].len());
    let mut visited: HashSet<(Pos, (i32, i32))> = HashSet::new();

    while let Some(next) = move_pos(&pos, &dir, &bounds) {
        if visited.contains(&(pos, dir)) {
            return true;
        }

        visited.insert((pos, dir));

        if field[next.0][next.1] == '#' {
            dir = match dir {
                (-1, 0) => (0, 1),
                (0, 1) => (1, 0),
                (1, 0) => (0, -1),
                (0, -1) => (-1, 0),
                _ => panic!(),
            };
        } else {
            pos = next;
            field[pos.0][pos.1] = 'X';
        }
    }

    false
}

fn solve(input: &str) -> (usize, usize) {
    let (start, mut field) = parse(input);

    walk(&start, &mut field);

    let a1 = field
        .iter()
        .map(|row| row.iter().filter(|&&c| c == 'X' || c == '^').count())
        .sum();

    let cols = field[0].len();

    let candidates = (0..field.len())
        .flat_map(|r| (0..cols).map(move |c| (r, c)))
        .filter(|&(r, c)| field[r][c] == 'X')
        .collect::<Vec<_>>();

    let total = candidates.len();
    let mut threadz = vec![];
    for window in candidates.chunks(total / 12) {
        let mut f = field.clone();
        let win = Vec::from(window);

        let t = thread::spawn(move || {
            win.iter()
                .filter(|pos| {
                    let prev = f[pos.0][pos.1];
                    f[pos.0][pos.1] = '#';
                    let has_loop = walk(&start, &mut f);
                    f[pos.0][pos.1] = prev;
                    has_loop
                })
                .count()
        });
        threadz.push(t);
    }

    let a2 = threadz.into_iter().map(|t| t.join().unwrap()).sum();

    (a1, a2)
}

fn main() {
    let i0 = fs::read_to_string("./06/input0").unwrap();
    println!("{:?}", solve(&i0));

    let i = fs::read_to_string("./06/input").unwrap();
    println!("{:?}", solve(&i));
}

fn p(field: &Vec<Vec<char>>) {
    for line in field {
        for c in line {
            print!("{}", c);
        }
        println!();
    }

    println!();
}

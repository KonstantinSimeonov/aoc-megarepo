use std::{fs, collections::HashMap};

static DELTAS: &[(i32, i32); 4] = &[
    (0, 1),
    (1, 0),
    (0, -1),
    (-1, 0)
];

fn parse_dirs(dirs: &str) -> Vec<(i32, i32)> {
    
    let mut res = Vec::new();
    let mut dir = 0;
    let mut forward = 0;

    for &x in dirs.as_bytes() {
        if x == b'R' {
            res.push((forward, dir));
            forward = 0;
            dir = 1;
        } else if x == b'L' {
            res.push((forward, dir));
            forward = 0;
            dir = -1;
        } else {
            forward = forward * 10 + (x - 48) as i32;
        }
    }

    res.push((forward, dir));

    res
}

fn index(y: i32, x: i32, board: &Vec<&[u8]>) -> Option<u8> {
    if y < 0 || x < 0 {
        return None;
    }

    let sy = y as usize;
    let sx = x as usize;

    if sy >= board.len() || sx >= board[sy].len() {
        return None;
    }

    Some(board[sy][sx])
}

#[allow(dead_code)]
fn render(board: &Vec<&[u8]>, visited: &HashMap<(i32, i32), String>) {
    for (y, row) in board.iter().enumerate() {
        for (x, &c) in row.iter().enumerate() {
            let v = if c == b' ' {
                " ".to_string()
            } else if let Some(d) = visited.get(&(y as i32, x as i32)) {
                d.clone()
            } else {
                (board[y][x] as char).to_string()
            };

            print!("{}", v);
        }

        println!();
    }
}

fn main() {
    let input = fs::read_to_string("./input").expect("gadno maze");

    let (raw_board, raw_dirs) = input.trim_end().split_once("\n\n").unwrap();

    let board = raw_board.lines().map(|line| line.as_bytes()).collect::<Vec<_>>();

    let dirs = parse_dirs(raw_dirs);
    let mut current = board[0]
        .iter()
        .enumerate()
        .find_map(|(i, &tile)| if tile == b'.' { Some((0, i as i32)) } else { None })
        .unwrap();

    let mut dir = 0;

    let mut visited = HashMap::new();

    let dl = DELTAS.len() as i32;

    visited.insert(current.clone(), [">", "v", "<", "^"][dir as usize].to_string());

    for &(forward, turn) in dirs.iter() {
        dir = (dl + dir + turn) % dl;
        let delta = DELTAS[dir as usize];
        let (dy, dx) = delta;

        let mut f = forward;
        while f > 0 {
            let ny = current.0 + dy;
            let nx = current.1 + dx;

            //println!("{:?}", current);

            match index(ny, nx, &board) {
                Some(b'.') => {
                    current = (ny, nx);
                    visited.insert((current.0, current.1), [">", "v", "<", "^"][dir as usize].to_string());
                },
                Some(b'#') => break,
                Some(b' ') | None => {
                    let rdy = dy * -1;
                    let rdx = dx * -1;

                    let mut wy = current.0 + rdy;
                    let mut wx = current.1 + rdx;

                    while let Some(c) = index(wy, wx, &board) {
                        if c == b' ' {
                            break;
                        }

                        wy += rdy;
                        wx += rdx;
                    }

                    wy -= rdy;
                    wx -= rdx;

                    if let Some(c) = index(wy, wx, &board) {
                        if c == b'.' {
                            current = (wy, wx);
                        }
                    }
                },
                _ => unreachable!()
            }

            f -= 1;
        }
    }



    render(&board, &visited);
    let part1 = (current.0 + 1) * 1000 + (current.1 + 1) * 4 + dir;
    println!("current {:?}, dir: {}", current, dir);
    println!("part1: {}", part1);
}

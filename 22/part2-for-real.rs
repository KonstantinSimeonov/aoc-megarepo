use std::{collections::HashMap, fs};

const R: i32 = 0;
const D: i32 = 1;
const L: i32 = 2;
const U: i32 = 3;
const DELTAS: &[(i32, i32); 4] = &[(0, 1), (1, 0), (0, -1), (-1, 0)];

fn parse_dirs(dirs_str: &str) -> Vec<(i32, i32)> {
    let mut dirs = Vec::new();
    let mut dir = 0;
    let mut forward = 0;

    for &x in dirs_str.as_bytes() {
        if x == b'R' {
            dirs.push((forward, dir));
            forward = 0;
            dir = 1;
        } else if x == b'L' {
            dirs.push((forward, dir));
            forward = 0;
            dir = -1;
        } else {
            forward = forward * 10 + (x - 48) as i32;
        }
    }

    dirs.push((forward, dir));

    dirs
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

const S: usize = 50;
const SI: i32 = S as i32;

fn transition(delta: &(i32, i32), (y, x): (i32, i32), q: usize) -> (i32, i32, i32) {
    let boog = |dir: &str| {
        println!(
            "bad {} transition -.- quad {}, y: {}, x: {}, delta: {:?}",
            dir, q, y, x, delta
        );
        unreachable!()
    };

    match delta {
        (0, 1) => match q {
            1 => (3 * SI - y - 1, 2 * SI - 1, L), // 4, left
            2 => (SI - 1, SI + y, U),             // 1, up
            4 => (3 * SI - 1 - y, 3 * SI - 1, L), // 1, left
            5 => (3 * SI - 1, y - 2 * SI, U),     // 4, up
            _ => boog("right"),
        },
        (1, 0) => match q {
            1 => (x - SI, 2 * SI - 1, L), // 2, left
            4 => (2 * SI + x, SI - 1, L), // 5, left
            5 => (0, 2 * SI + x, D),      // boog? 1, down
            _ => boog("down"),
        },
        (-1, 0) => match q {
            0 => (x + 2 * SI, 0, R),          // 5, right
            1 => (4 * SI - 1, x - 2 * SI, U), // 5, up
            3 => (SI + x, SI, R),             // 2, right
            _ => boog("up"),
        },
        (0, -1) => match q {
            0 => (3 * SI - 1 - y, 0, R),  // 3, right
            2 => (2 * SI, y - SI, D),     // 3, down
            3 => (3 * SI - 1 - y, SI, R), // 0, right
            5 => (0, y - 2 * SI, D),      // 0, down
            _ => boog("left"),
        },
        _ => {
            println!("what even is this delta {:?}", delta);
            unreachable!()
        }
    }
}

fn calc_quad(y: i32, x: i32) -> usize {
    let q = [
        (0..S, S..2 * S),
        (0..S, 2 * S..3 * S),
        (S..2 * S, S..2 * S),
        (2 * S..3 * S, 0..S),
        (2 * S..3 * S, S..2 * S),
        (3 * S..4 * S, 0..S),
    ]
    .iter()
    .position(|(ry, rx)| ry.contains(&(y as usize)) && rx.contains(&(x as usize)));

    match q {
        None => {
            println!("calc_quad fail {} {}", y, x);
            unreachable!()
        }
        Some(x) => x,
    }
}

fn main() {
    // test_transitions();

    let input = fs::read_to_string("./input").expect("gadno maze");
    let (raw_board, raw_dirs) = input.trim_end().split_once("\n\n").unwrap();
    let board = raw_board
        .lines()
        .map(|line| line.as_bytes())
        .collect::<Vec<_>>();

    let dirs = parse_dirs(raw_dirs);
    let mut current = board[0]
        .iter()
        .enumerate()
        .find_map(|(i, &tile)| {
            if tile == b'.' {
                Some((0, i as i32))
            } else {
                None
            }
        })
        .unwrap();

    let dl = DELTAS.len() as i32;
    let mut dir = 0;

    let mut visited = HashMap::new();
    let mut visit = |(y, x): &(i32, i32), dir: i32| {
        visited.insert((*y, *x), [">", "v", "<", "^"][dir as usize].to_string());
    };

    for &(forward, turn) in dirs.iter() {
        dir = (dl + dir + turn) % dl;

        visit(&current, dir);

        let mut steps_left = forward;
        while steps_left > 0 {
            let delta = DELTAS[dir as usize];
            let (dy, dx) = delta;
            let ny = current.0 + dy;
            let nx = current.1 + dx;

            match index(ny, nx, &board) {
                Some(b'.') => {
                    current = (ny, nx);
                    visit(&current, dir);
                }
                Some(b'#') => break,
                Some(b' ') | None => {
                    let ly = ny - dy;
                    let lx = nx - dx;
                    let (ty, tx, ndir) = transition(&delta, (ly, lx), calc_quad(ly, lx));
                    if let Some(c) = index(ty, tx, &board) {
                        assert_ne!(c, b' ');
                        if c == b'.' {
                            current = (ty, tx);
                            dir = ndir;
                            visit(&current, dir);
                        }
                    }
                }
                _ => unreachable!(),
            }

            steps_left -= 1;
        }
    }

    //render(&board, &visited);
    let part2 = (current.0 + 1) * 1000 + (current.1 + 1) * 4 + dir;
    println!("current {:?}, dir: {}", current, dir);
    println!("part2: {}", part2);
}

fn render(board: &Vec<&[u8]>, visited: &HashMap<(i32, i32), String>) {
    for (y, row) in board.iter().enumerate() {
        for (x, &c) in row.iter().enumerate() {
            //if c == b' ' {
            //    printn!("{}", c);
            //    continue;
            //}
            //let c1 = quad0(y as i32, x as i32).to_string();
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

fn test_transitions() {
    println!("right 1");
    let coords1 = (0..S).map(|y| (y as i32, (3 * S - 1) as i32));
    for c in coords1 {
        assert_eq!(calc_quad(c.0, c.1), 1);
        let ts = transition(&(0, 1), c, 1);
        println!("{:?} {:?}", c, ts);
        assert_eq!(calc_quad(ts.0, ts.1), 4);
    }

    println!("\nright 2");
    let coords2 = (S..2 * S).map(|y| (y as i32, 2 * SI - 1));
    for c in coords2 {
        assert_eq!(calc_quad(c.0, c.1), 2);
        let ts = transition(&(0, 1), c, 2);
        println!("{:?} {:?}", c, ts);
        assert_eq!(calc_quad(ts.0, ts.1), 1);
    }

    println!("\nright 4");
    let coords3 = (2 * S..3 * S).map(|y| (y as i32, 2 * SI - 1));
    for c in coords3 {
        assert_eq!(calc_quad(c.0, c.1), 4);
        let ts = transition(&(0, 1), c, 4);
        println!("{:?} {:?}", c, ts);
        assert_eq!(calc_quad(ts.0, ts.1), 1);
    }

    println!("\nright 5");
    let coords4 = (3 * S..4 * S).map(|y| (y as i32, SI - 1));
    for c in coords4 {
        assert_eq!(calc_quad(c.0, c.1), 5);
        let ts = transition(&(0, 1), c, 5);
        println!("{:?} {:?}", c, ts);
        assert_eq!(calc_quad(ts.0, ts.1), 4);
    }

    println!("\ndown 1");
    let coords5 = (2 * S..3 * S).map(|x| (SI - 1, x as i32));
    for c in coords5 {
        assert_eq!(calc_quad(c.0, c.1), 1);
        let ts = transition(&(1, 0), c, 1);
        println!("{:?} {:?}", c, ts);
        assert_eq!(calc_quad(ts.0, ts.1), 2);
    }

    println!("\ndown 4");
    let coords6 = (S..2 * S).map(|x| (3 * SI - 1, x as i32));
    for c in coords6 {
        assert_eq!(calc_quad(c.0, c.1), 4);
        let ts = transition(&(1, 0), c, 4);
        println!("{:?} {:?}", c, ts);
        assert_eq!(calc_quad(ts.0, ts.1), 5);
    }

    println!("\ndown 5");
    let coords7 = (0..S).map(|x| (4 * SI - 1, x as i32));
    for c in coords7 {
        assert_eq!(calc_quad(c.0, c.1), 5);
        let ts = transition(&(1, 0), c, 5);
        println!("{:?} {:?}", c, ts);
        assert_eq!(calc_quad(ts.0, ts.1), 1);
    }

    println!("\nup 0");
    let coords8 = (S..2 * S).map(|x| (0, x as i32));
    for c in coords8 {
        assert_eq!(calc_quad(c.0, c.1), 0);
        let ts = transition(&(-1, 0), c, 0);
        println!("{:?} {:?}", c, ts);
        assert_eq!(calc_quad(ts.0, ts.1), 5);
    }

    println!("\nup 1");
    let coords9 = (2 * S..3 * S).map(|x| (0, x as i32));
    for c in coords9 {
        assert_eq!(calc_quad(c.0, c.1), 1);
        let ts = transition(&(-1, 0), c, 1);
        println!("{:?} {:?}", c, ts);
        assert_eq!(calc_quad(ts.0, ts.1), 5);
    }

    println!("\nup 3");
    let coords10 = (0..S).map(|x| (2 * SI, x as i32));
    for c in coords10 {
        assert_eq!(calc_quad(c.0, c.1), 3);
        let ts = transition(&(-1, 0), c, 3);
        println!("{:?} {:?}", c, ts);
        assert_eq!(calc_quad(ts.0, ts.1), 2);
    }

    println!("\nleft 0");
    let coords11 = (0..S).map(|y| (y as i32, SI));
    for c in coords11 {
        assert_eq!(calc_quad(c.0, c.1), 0);
        let ts = transition(&(0, -1), c, 0);
        println!("{:?} {:?}", c, ts);
        assert_eq!(calc_quad(ts.0, ts.1), 3);
    }

    println!("\nleft 2");
    let coords12 = (S..2 * S).map(|y| (y as i32, SI));
    for c in coords12 {
        assert_eq!(calc_quad(c.0, c.1), 2);
        let ts = transition(&(0, -1), c, 2);
        println!("{:?} {:?}", c, ts);
        assert_eq!(calc_quad(ts.0, ts.1), 3);
    }

    println!("\nleft 3");
    let coords13 = (2 * S..3 * S).map(|y| (y as i32, 0));
    for c in coords13 {
        assert_eq!(calc_quad(c.0, c.1), 3);
        let ts = transition(&(0, -1), c, 3);
        println!("{:?} {:?}", c, ts);
        assert_eq!(calc_quad(ts.0, ts.1), 0);
    }

    println!("\nleft 5");
    let coords14 = (3 * S..4 * S).map(|y| (y as i32, 0));
    for c in coords14 {
        assert_eq!(calc_quad(c.0, c.1), 5);
        let ts = transition(&(0, -1), c, 5);
        println!("{:?} {:?}", c, ts);
        assert_eq!(calc_quad(ts.0, ts.1), 0);
    }

    println!("passed")
}

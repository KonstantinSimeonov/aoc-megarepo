use std::{
    collections::{HashSet, VecDeque},
    fs,
};

type P = (i32, i32);

const U: P = (-1, 0);
const D: P = (1, 0);
const L: P = (0, -1);
const R: P = (0, 1);
const STAY: P = (0, 0);

fn main() {
    let input = fs::read_to_string("./input").expect("blizzy in da hizzy");

    let mut size_y = -2;
    let mut size_x = 0;

    let mut blizzys = input
        .trim()
        .lines()
        .enumerate()
        .flat_map(|(i, line)| {
            size_y += 1;
            size_x = line.len() as i32 - 2;
            line.chars().enumerate().filter_map(move |(j, c)| {
                let pos = ((i as i32) - 1, (j as i32) - 1);
                match c {
                    '>' => Some((pos, R)),
                    '<' => Some((pos, L)),
                    'v' => Some((pos, D)),
                    '^' => Some((pos, U)),
                    _ => None,
                }
            })
        })
        .collect::<Vec<_>>();

    println!("rows: {}, cols: {}", size_y, size_x);

    let mut frees = vec![invert_rect(size_y, size_x, &blizzys)];
    for _ in 1..1500 {
        let s = blizzys
            .iter()
            .map(|((y, x), delta)| {
                (
                    (
                        (size_y + y + delta.0) % size_y,
                        (size_x + x + delta.1) % size_x,
                    ),
                    *delta,
                )
            })
            .collect::<Vec<_>>();

        frees.push(invert_rect(size_y, size_x, &s));
        blizzys = s;
    }

    let pat = find_repeating_pattern(&frees).unwrap();
    println!("blizzard pattern detected {:?}", pat);
    let fs = &frees[pat.0..pat.1];

    let (_, time_to_exit) = walk(&fs, (-1, 0), 1, (size_y - 1, size_x - 1))
        .expect("to get out eventually");
    println!("part1: {}", time_to_exit);

    let (_, time_to_entrance) = walk(
        &fs,
        (size_y, size_x - 2),
        time_to_exit + 1,
        (0, 0),
    )
    .expect("to get back to start");
    println!("part2: {:?}", time_to_entrance);

    let (_, part2) = walk(
        &fs,
        (0, 0),
        time_to_entrance + 1,
        (size_y - 1, size_x - 1),
    )
    .expect("to get out a second time");

    println!("part2: {}", part2)
}

fn walk(
    frees: &[HashSet<P>],
    start: P,
    start_time: usize,
    end: P,
) -> Option<(P, usize)> {
    let mut visited = HashSet::new();

    let mut nodes = VecDeque::from([(start, start_time)]);
    while let Some(curr) = nodes.pop_front() {
        let ((y, x), time) = curr;
        let f = &frees[time % frees.len()];

        let next = [U, D, L, R, STAY]
            .iter()
            .map(|(dy, dx)| (y + dy, x + dx))
            .filter(|p| f.contains(p))
            .map(|p| (p, time + 1));

        for n in next {
            if n.0 == end {
                return Some(n);
            }

            if visited.insert((n.0, n.1 % frees.len())) {
                nodes.push_back(n);
            }
        }
    }

    None
}

fn invert_rect(size_y: i32, size_x: i32, blizzys: &Vec<(P, P)>) -> HashSet<P> {
    let mut free = (0..size_y as usize)
        .flat_map(|i| (0..size_x as usize).map(move |j| (i as i32, j as i32)))
        .collect::<HashSet<_>>();

    for (pos, _) in blizzys.iter() {
        free.remove(pos);
    }

    free.insert((-1, 0));
    free.insert((size_y, size_x - 2));

    free
}

fn find_repeating_pattern<T: Eq>(xs: &Vec<T>) -> Option<(usize, usize, usize)> {
    for i in 0..xs.len() {
        let mut j = i + 1;

        while j < xs.len() {
            if xs[i] == xs[j] {
                let mut j1 = j;
                let mut i1 = i;

                while j1 < xs.len() && xs[i1] == xs[j1] {
                    i1 += 1;
                    j1 += 1;
                }

                if j1 == xs.len() && i1 > j {
                    return Some((i, j, (j1 - i) / (j1 - i1)));
                }
            }

            j += 1;
        }
    }

    None
}

#[allow(dead_code)]
fn render(size_y: i32, size_x: i32, blizzys: &HashSet<P>) {
    let mut y = 0;
    println!("{}", "#".repeat(size_x as usize + 2));
    while y < size_y {
        let mut x = 0;
        print!("#");
        while x < size_x {
            print!("{}", if blizzys.contains(&(y, x)) { "." } else { "*" });
            x += 1;
        }

        println!("#");

        y += 1;
    }
    println!("{}", "#".repeat(size_x as usize + 2));
}

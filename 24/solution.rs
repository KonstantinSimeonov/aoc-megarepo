use std::{fs, collections::{HashSet, VecDeque}};

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

    let blizzys = input
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

    println!("{} {}", size_y, size_x);

    let mut frees = vec![invert_rect(size_y, size_x, &blizzys)];
    let mut states = vec![blizzys];

    for i in 1..2000 {
        let s = states[i - 1]
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
        states.push(s);
    }

    let pat = find_repeating_pattern(&frees).unwrap();
    println!("pat {:?}", pat);
    let fs = &frees[pat.0 as usize..pat.1 as usize];

    let mut visited = HashSet::new();

    let mut nodes = VecDeque::from([((-1, 0), 1)]);
    let mut r = 0;
    while let Some(curr) = nodes.pop_front() {
        r = r + 1;
        if r % 2000 == 0 {
            //println!("prg {} {}", r, nodes.len());
        }
        let ((y, x), time) = curr;
        let f = &fs[time % fs.len()];
        // println!("current {:?} {:?}", curr, f);

        let next = [U, D, L, R, STAY]
            .iter()
            .map(|(dy, dx)| (y + dy, x + dx))
            .filter(|p| f.contains(p))
            .map(|p| (p, time + 1));

        for n in next {
            // println!("cue {:?} {:?}", curr, n);
            if n.0 == (size_y - 1, size_x - 1) {
                println!("finish {:?}", n);
                return;
            }
            if visited.insert((n.0, n.1 % fs.len())) {
                nodes.push_back(n);
            }
        }

        // println!();
    }

    panic!("close but no cigar");
}

fn invert_rect(size_y: i32, size_x: i32, blizzys: &Vec<(P, P)>) -> HashSet<P> {
    let mut free = (0..size_y as usize)
        .flat_map(|i| (0..size_x as usize).map(move |j| (i as i32, j as i32)))
        .collect::<HashSet<_>>();

    for (pos, _) in blizzys.iter() {
        free.remove(pos);
    }

    free.insert((-1, 0));

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

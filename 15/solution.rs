use std::{cmp, collections::HashSet, fs};
use std::thread;

type P2 = (i32, i32);

fn main() {
    let (file, y, max) = [("./input0", 10, 20), ("./input", 2000000, 4000000)][1];

    let input = fs::read_to_string(file).expect("something");

    let pairs = input
        .trim_end()
        .split("\n")
        .map(|line| {
            let sl = line
                .split(['=', ':', ','])
                .skip(1)
                .step_by(2)
                .map(|coord| coord.parse::<i32>().unwrap())
                .collect::<Vec<i32>>();

            match sl[..] {
                [x, y, bx, by] => ((x, y), (bx, by)),
                _ => {
                    unreachable!()
                }
            }
        })
        .collect::<Vec<(P2, P2)>>();

    let part1 = {
        let xs = calc_empty(y, &pairs);

        let beacon_xs = pairs
            .iter()
            .filter_map(|(_, (bx, by))| if *by == y { Some((bx, by)) } else { None })
            .filter(|(bx, _)| xs.iter().any(|(s, e)| s <= bx && bx <= &e))
            .collect::<HashSet<_>>();

        let result = xs.iter().fold(0, |sum, (s, e)| sum + e - s + 1) - beacon_xs.len() as i32;

        result
    };
    println!("part 1: {}", part1);

    let part2 = {
        let l = thread::available_parallelism().unwrap().get();
        let threads = (1..l)
            .map(|i| {
                let start = i * max / l;
                let end = (i + 1) * max / l;

                let ps = pairs.clone();
                thread::spawn(move || run_range(&ps, start, end))
            })
            .collect::<Vec<_>>();

        let result = threads
            .into_iter()
            .find_map(|t| t.join().unwrap())
            .or(run_range(&pairs, 0, max / l));

        result
    };
    println!("{:?}", part2)
}

fn run_range(pairs: &Vec<(P2, P2)>, s: usize, e: usize) -> Option<u128> {
    let res = (s..e).find_map(|ty| {
            let xs = calc_empty(ty as i32, &pairs);

            match xs[..] {
                [(_, e), _] => Some((e + 1, ty)),
                _ => None
            }
        });

    res.map(|(dbx, dby)| {
        let x = dbx as u128;
        let y = dby as u128;
        (x * 4000000) + y
    })
}

fn calc_empty(y: i32, pairs: &Vec<(P2, P2)>) -> Vec<(i32, i32)> {
    let mut all = pairs
        .iter()
        .filter_map(|p| isect(y, p))
        .collect::<Vec<P2>>();

    all.sort();

    let xs = all.into_iter().fold(vec![], |mut rs, (s, e)| {
        let l = rs.len();
        if l == 0 {
            rs.push((s, e));
            rs
        } else {
            let (s1, e1) = rs.last().unwrap();

            if *e1 >= s - 1 {
                rs[l - 1] = (*s1, cmp::max(*e1, e));
            } else {
                rs.push((s, e));
            }

            rs
        }
    });

    xs
}

fn isect(y: i32, (s, b): &(P2, P2)) -> Option<P2> {
    let mdist = (s.0 - b.0).abs() + (s.1 - b.1).abs();
    let ydist = (y - s.1).abs();
    if mdist < ydist {
        //println!("{:?}", (mdist, ydist, s, b));
        return None;
    }

    let d = mdist - ydist;
    let start = s.0 - d;
    let end = s.0 + d;

    Some((start, end))
}

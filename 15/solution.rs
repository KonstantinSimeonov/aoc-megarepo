use std::{fs, collections::HashSet, cmp};

type P2 = (i32, i32);

fn main() {
    let (file, y) = [
        ("./input0", 10),
        ("./input", 2000000)
    ][1];

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

    let part1 = calc_empty(y, &pairs);
    println!("part 1: {}", part1)
}

fn calc_empty(y: i32, pairs: &Vec<(P2, P2)>) -> i32 {
    let mut all = pairs.iter().filter_map(|p| isect(y, p)).collect::<Vec<P2>>();

    all.sort();

    let xs = all
        .iter()
        .fold(vec![], |mut rs, (s, e)| {
            let l = rs.len();
            if l == 0 {
                rs.push((s, e));
                rs
            } else {
                let (s1, e1) = rs.last().unwrap();

                if e1 >= &s {
                    rs[l - 1] = (s1, cmp::max(e1, e));
                } else {
                    rs.push((s, e));
                }

                rs
            }
        });

    let beacon_xs = pairs
        .iter()
        .filter_map(|(_, (bx, by))| if *by == y { Some((bx, by)) } else { None })
        .filter(|(bx, _)| xs.iter().any(|(s, e)| s <= bx && bx <= e))
        .collect::<HashSet<_>>();

    let part1 = xs
        .iter()
        .fold(0, |sum, (&s, &e)| sum + e - s + 1) - beacon_xs.len() as i32;

    part1
}

fn isect(y: i32, (s, b): &(P2, P2)) -> Option<P2> {
    let mdist = (s.0 - b.0).abs() + (s.1 - b.1).abs();
    let ydist = (y - s.1).abs();
    if mdist < ydist {
        //println!("{:?}", (mdist, ydist, s, b));
        return None
    }

    let d = mdist - ydist;
    let start = s.0 - d;
    let end = s.0 + d;

    Some((start, end))
}

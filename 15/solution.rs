use std::{fs, collections::HashSet};

type P2 = (i32, i32);

fn main() {
    let input = fs::read_to_string("./input").expect("something");

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

    //println!("{:?}", pairs);

    let y = 2000000;
    let mut all = pairs.iter().filter_map(|p| isect(y, p)).collect::<Vec<P2>>();

    all.sort();

    let bs = pairs.iter().filter_map(|(_, (bx, by))| if *by == y { Some(bx) } else { None });
    let mut hs = all.iter().flat_map(|r| (r.0..r.1 + 1)).collect::<HashSet<i32>>();

    for beacon in bs.clone() {
        hs.remove(beacon);
    }
    println!("{:?}", (hs.len()))
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

use std::cmp;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;

fn main() {
    let input = fs::read_to_string("./input").expect("pipezzz boii");

    let mut cnt = 1;
    let mut pipes_names = HashMap::new();
    let mut add_key = |name: &str| -> i32 {
        let k = if !pipes_names.contains_key(name) {
            let old = cnt;
            cnt *= 2;
            pipes_names.insert(name.to_string(), old);
            old
        } else {
            *pipes_names.get(name).unwrap()
        };

        k
    };

    let ps = parse(&input);

    let mut ks = ps
        .iter()
        .filter_map(|(k, (rate, _))| {
            if *rate == 0 && *k != "AA" {
                None
            } else {
                Some(*k)
            }
        })
        .collect::<Vec<_>>();

    ks.sort();

    for x in ks.iter() {
        add_key(x);
    }

    let graph = ks
        .iter()
        .map(|k| {
            let (rate, ns) = neighbs(&ps, k);
            //println!("{:?}", (k, add_key(k)));
            let keyed = ns
                .iter()
                .map(|(name, dist, rate)| (add_key(name), *dist, *rate))
                .collect::<Vec<_>>();

            (add_key(k), (rate, keyed))
        })
        .collect::<HashMap<_, _>>();

    println!(
        "part1: {}",
        run_all_paths(&graph, 1, 30, 0, 0, &mut HashMap::new())
    );

    let mut all_path_scores = HashMap::new();
    run_all_paths(&graph, 1, 26, 0, 0, &mut all_path_scores);

    let slona = all_path_scores
        .iter()
        .flat_map(|r1| all_path_scores.iter().map(move |r2| (r1, r2)))
        .filter(|(r1, r2)| r1.0 & r2.0 == 1)
        .map(|(r1, r2)| r1.1 + r2.1)
        .max()
        .unwrap();

    println!("part2: {}", slona);
}

type RawGraph<'a> = HashMap<&'a str, (i32, Vec<&'a str>)>;

fn parse<'a>(input: &'a str) -> RawGraph {
    let paths = input
        .lines()
        .map(|line| {
            let mut l = line.split(';');
            let f = l.next().unwrap();
            let s = l.next().unwrap();

            let (name, rate) = match f.split(' ').collect::<Vec<_>>()[..] {
                [_, name, _, _, end] => (name, end.strip_prefix("rate=").unwrap()),
                _ => unreachable!(),
            };

            let tunnels = s.split(' ').skip(5).map(|t| &t[0..2]).collect::<Vec<_>>();

            (name, (rate.parse::<i32>().unwrap(), tunnels))
        })
        .collect::<HashMap<_, _>>();

    paths
}

fn rem(t: i32, c: i32) -> i32 {
    t & !c
}

fn insert(t: i32, c: i32) -> i32 {
    //assert!(t < 1 << 16);
    //assert!(c < (1 << 16));
    //assert!(t & c == 0);
    t | c
}

fn has(t: i32, c: i32) -> bool {
    //assert!(t < 1 << 16);
    //assert!(c < (1 << 16));
    t & c != 0
}

// (1600..1626)
fn run_all_paths<'a>(
    net: &HashMap<i32, (i32, Vec<(i32, i32, i32)>)>,
    current: i32,
    left: i32,
    turned: i32,
    score: i32,
    ps: &mut HashMap<i32, i32>,
) -> i32 {
    let (_, next) = net.get(&current).unwrap();

    let new_turned = insert(turned, current);

    ps.entry(new_turned)
        .and_modify(|x| *x = cmp::max(*x, score))
        .or_insert(score);

    let result = next
        .iter()
        .filter_map(|(node, cost, rate)| {
            if has(new_turned, *node) {
                return None;
            }

            let new_left = left - cost - 1;
            if new_left < 0 {
                return None;
            }

            let released = rate * new_left;

            Some(released + run_all_paths(net, *node, new_left, new_turned, score + released, ps))
        })
        .max()
        .unwrap_or(0);

    result
}

fn neighbs<'a>(
    net: &RawGraph<'a>,
    current: &'a str,
) -> (i32, Vec<(&'a str, i32, i32)>) {
    let mut visited = HashSet::new();
    let mut nodes = VecDeque::from([(current, 0)]);
    let mut result = Vec::new();
    let (rate, _) = net.get(current).unwrap();

    visited.insert(current);

    while let Some((c, dist)) = nodes.pop_front() {
        let (r, cs) = net.get(c).unwrap();
        if *r != 0 && c != current {
            result.push((c, dist, *r));
        }

        for c in cs.iter() {
            if !visited.contains(c) {
                visited.insert(c);
                nodes.push_back((c, dist + 1));
            }
        }
    }

    (*rate, result)
}

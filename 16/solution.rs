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

    let ps = input
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
        "kekw {}",
        max_release_nice(&graph, 1, 30, 0)
    );
}

fn insert(t: i32, c: i32) -> i32 {
    assert!(t < 1 << 16);
    assert!(c < (1 << 16));
    assert!(t & c == 0);
    t | c
}

fn has(t: i32, c: i32) -> bool {
    assert!(t < 1 << 16);
    assert!(c < (1 << 16));
    t & c != 0
}

// (1600..1626)
fn max_release_nice<'a>(
    net: &HashMap<i32, (i32, Vec<(i32, i32, i32)>)>,
    current: i32,
    left: i32,
    turned: i32,
) -> i32 {
    let (_, next) = net.get(&current).unwrap();

    let t = insert(turned, current);
    let press = next
        .iter()
        .filter_map(|(name, cost, rate)| {
            if has(t, *name) {
                return None;
            }
            let l = left - cost - 1;
            if l < 0 {
                return None;
            }

            let p = rate * l;

            Some(p + max_release_nice(net, *name, l, t))
        })
        .max()
        .unwrap_or(0);

    press
}

fn neighbs<'a>(
    net: &HashMap<&'a str, (i32, Vec<&'a str>)>,
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

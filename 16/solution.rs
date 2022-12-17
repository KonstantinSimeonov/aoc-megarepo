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
        }).collect::<Vec<_>>();

    ks.sort();

    let start = 1;
    let graph = ks.iter()
        .map(|k| {
            let (rate, ns) = neighbs(&ps, k);
            add_key(k);
            let keyed = ns
                .iter()
                .map(|(name, dist)| (add_key(name), *dist))
                .collect::<Vec<(i32, i32)>>();

            (add_key(k), (rate, keyed))
        })
        .collect::<HashMap<_, _>>();

    //println!("{:?}", {
    //    let mut ks = graph.keys().collect::<Vec<_>>();
    //    ks.sort();
    //    ks
    //});

    //println!("{}", graph.iter().map(|(k, v)| "" + k.to_string() + " -> " + v.to_string()));

    println!("{}, {:?}", graph.len(), {
        let mut xs = graph.iter().map(|(k, v)| {
            let mut v1 = v.1.clone();
            v1.sort();
            (k, (v.0, v1))
        }).collect::<Vec<_>>();
        xs.sort();
        xs
    });
    println!(
        "{}",
        max_release_nice(&graph, start, 30, 0, &mut HashMap::new())
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
    net: &HashMap<i32, (i32, Vec<(i32, i32)>)>,
    current: i32,
    left: i32,
    turned: i32,
    cache: &mut HashMap<(i32, i32, i32), i32>,
) -> i32 {
    if let Some(res) = cache.get(&(left, turned, current)) {
        return *res;
    }

    if left <= 0 {
        return 0;
    }

    let (rate, next) = net.get(&current).unwrap();

    let with = if has(turned, current) {
        0
    } else {
        let l = left - 1;
        let p = rate * l;
        let press = next
            .iter()
            .map(|(name, cost)| {
                max_release_nice(net, *name, l - cost, insert(turned, current), cache)
            })
            .max()
            .unwrap();

        p + press
    };

    let without = next
        .iter()
        .map(|(name, cost)| max_release_nice(net, *name, left - cost, turned, cache))
        .max()
        .unwrap();

    let res = cmp::max(with, without);
    cache.insert((left, turned, current), res);

    res
}

fn neighbs<'a>(
    net: &HashMap<&'a str, (i32, Vec<&'a str>)>,
    current: &'a str,
) -> (i32, Vec<(&'a str, i32)>) {
    let mut visited = HashSet::new();
    let mut nodes = VecDeque::from([(current, 0)]);
    let mut result = Vec::new();
    let (rate, _) = net.get(current).unwrap();

    visited.insert(current);

    while let Some((c, dist)) = nodes.pop_front() {
        let (r, cs) = net.get(c).unwrap();
        if *r != 0 && c != current {
            result.push((c, dist));
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

// slow af
fn max_release<'a>(
    net: &HashMap<&str, (i32, Vec<&'a str>)>,
    current: &'a str,
    left: i32,
    visited: &mut HashSet<&'a str>,
) -> i32 {
    //println!("{}", left);
    if left == 0 || visited.len() == net.len() {
        return 0;
    };

    let (rate, next) = net.get(current).unwrap();

    if *rate == 0 || visited.contains(current) {
        let m = next
            .iter()
            .map(|t| max_release(net, t, left - 1, visited))
            .max()
            .unwrap();

        return m;
    }

    visited.insert(current);
    let score = rate * (left - 1);
    let with_turn = if left > 1 {
        next.iter()
            .map(|t| max_release(net, t, left - 2, visited))
            .max()
            .unwrap()
    } else {
        0
    };

    visited.remove(current);

    score + with_turn
}

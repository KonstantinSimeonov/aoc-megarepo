use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::cmp;

fn main() {
    let input = fs::read_to_string("./input0").expect("pipezzz boii");

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

    let graph = ps
        .iter()
        .filter_map(|(k, (rate, _))| if *rate == 0 && k != &"AA" { None } else { Some(*k) })
        .map(|k| (k, neighbs(&ps, k)))
        .collect::<HashMap<_, _>>();

    println!("{}, {:?}", graph.len(), graph);
    //println!("{}", max_release_nice(&graph, "AA", 30, &mut HashSet::new()));
    
}

fn max_release_nice<'a>(
    net: &HashMap<&'a str, (i32, Vec<(&'a str, i32, i32)>)>,
    current: &'a str,
    left: i32,
    turned: &mut HashSet<&'a str>,
) -> i32 {
    if left <= 0 || turned.len() >= net.len() - 1 {
        return 0
    }

    let (rate, next) = net.get(current).unwrap();

    let with = if turned.contains(current) {
        0
    } else if left == 1 {
        *rate
    } else {
        turned.insert(current);
        let l = left - 1;
        let p = rate * l;
        let press = next
            .iter()
            .map(|(name, r, cost)| max_release_nice(net, name, l - cost, turned))
            .max()
            .unwrap();

        turned.remove(current);

        p + press
    };

    let without = next
        .iter()
        .map(|(name, r, cost)| max_release_nice(net, name, left - cost, turned))
        .max()
        .unwrap();

    cmp::max(with, without)
}

fn neighbs<'a>(
    net: &HashMap<&str, (i32, Vec<&'a str>)>,
    current: &'a str
) -> (i32, HashMap<&'a str, i32>) {
    let mut visited = HashSet::new();
    let mut nodes = VecDeque::from([(current, 0)]);
    let mut result = HashMap::new();
    let (rate, _) = net.get(current).unwrap();

    visited.insert(current);

    while let Some((c, dist)) = nodes.pop_front() {
        let (r, cs) = net.get(c).unwrap();
        if *r != 0 && c != current {
            result.insert(c, dist);
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

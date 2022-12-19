use std::cmp;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;

type RawGraph<'a> = HashMap<&'a str, (i32, Vec<&'a str>)>;

type SimplifiedGraph = HashMap<i32, Vec<(i32, i32, i32)>>;

fn main() {
    let input = fs::read_to_string("./input").expect("pipezzz boii");

    let raw_graph = parse(&input);

    let keys = valve_keys(&raw_graph);

    let graph: SimplifiedGraph = keys
        .iter()
        .map(|(valve_name, int_key)| {
            let paths = paths_to_all_other(&raw_graph, valve_name);
            let keyed = paths
                .into_iter()
                .map(|(name, dist, rate)| (*keys.get(name).unwrap(), dist, rate))
                .collect::<Vec<_>>();

            (*int_key, keyed)
        })
        .collect::<HashMap<_, _>>();

    let mut paths1 = HashMap::new();
    run_all_paths(&graph, 1, 30, 0, 0, &mut paths1);
    println!("part1: {:?}", paths1.into_values().max());

    let mut paths2 = HashMap::new();
    run_all_paths(&graph, 1, 26, 0, 0, &mut paths2);

    let paths_vec = paths2.into_iter().collect::<Vec<_>>();
    let mut max = 0;
    for i in 0..paths_vec.len() - 1 {
        for j in (i + 1)..paths_vec.len() - 1 {
            if paths_vec[i].0 & paths_vec[j].0 == 1 {
                max = cmp::max(paths_vec[i].1 + paths_vec[j].1, max);
            }
        }
    }

    println!("{}", max)
}

fn valve_keys<'a>(graph: &RawGraph<'a>) -> HashMap<&'a str, i32> {
    let mut valve_names = graph
        .iter()
        .filter_map(|(name, (rate, _))| {
            if *rate == 0 && *name != "AA" {
                None
            } else {
                Some(*name)
            }
        })
        .collect::<Vec<_>>();

    valve_names.sort();

    valve_names
        .into_iter()
        .enumerate()
        .map(|(power, key)| (key, 1 << power))
        .collect::<HashMap<_, _>>()
}

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

fn run_all_paths<'a>(
    graph: &SimplifiedGraph,
    current: i32,
    left: i32,
    turned: i32,
    score: i32,
    all_paths: &mut HashMap<i32, i32>,
) {
    let paths = graph.get(&current).unwrap();
    let new_turned = insert(turned, current);

    all_paths.entry(new_turned)
        .and_modify(|x| *x = cmp::max(*x, score))
        .or_insert(score);

    for &(node, cost, rate) in paths.iter() {
        if has(new_turned, node) {
            continue;
        }

        let new_left = left - cost - 1;
        if new_left < 0 {
            continue;
        }

        let released = rate * new_left;
        run_all_paths(graph, node, new_left, new_turned, score + released, all_paths)
    }
}

fn paths_to_all_other<'a>(graph: &RawGraph<'a>, from: &'a str) -> Vec<(&'a str, i32, i32)> {
    let mut visited = HashSet::from([from]);
    let mut nodes = VecDeque::from([(from, 0)]);
    let mut paths = Vec::new();

    while let Some((current, distance)) = nodes.pop_front() {
        let (rate, neighbs) = graph.get(current).unwrap();
        if *rate != 0 && current != from {
            paths.push((current, distance, *rate));
        }

        for c in neighbs.iter() {
            if !visited.contains(c) {
                visited.insert(c);
                nodes.push_back((c, distance + 1));
            }
        }
    }

    paths
}

use std::cmp;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;

fn main() {
    let input = fs::read_to_string("./input").expect("pipezzz boii");

    let raw_graph = parse(&input);

    let keys = valve_keys(&raw_graph);

    let graph = keys
        .iter()
        .map(|(valve_name, int_key)| {
            let (rate, ns) = paths_to_all_other(&raw_graph, valve_name);
            let keyed = ns
                .iter()
                .map(|(name, dist, rate)| (*keys.get(name).unwrap(), *dist, *rate))
                .collect::<Vec<_>>();

            (*int_key, (rate, keyed))
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

type SimplifiedGraph = HashMap<i32, (i32, Vec<(i32, i32, i32)>)>;

type RawGraph<'a> = HashMap<&'a str, (i32, Vec<&'a str>)>;

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
    ps: &mut HashMap<i32, i32>,
) -> i32 {
    let (_, next) = graph.get(&current).unwrap();

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

            Some(released + run_all_paths(graph, *node, new_left, new_turned, score + released, ps))
        })
        .max()
        .unwrap_or(0);

    result
}

fn paths_to_all_other<'a>(graph: &RawGraph<'a>, from: &'a str) -> (i32, Vec<(&'a str, i32, i32)>) {
    let mut visited = HashSet::from([from]);
    let mut nodes = VecDeque::from([(from, 0)]);
    let mut paths = Vec::new();
    let (from_rate, _) = graph.get(from).unwrap();

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

    (*from_rate, paths)
}

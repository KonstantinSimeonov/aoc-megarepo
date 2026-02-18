use std::collections::{HashMap};

use aoc2026::Inputs;

fn main() {
    let inputs = Inputs::read();
    println!("==== input 0 ====");
    solve(&inputs.test_inputs[0]);
    println!("==== input 1 ====");
    solve(&inputs.test_inputs[1]);
    println!("==== input ====");
    solve(&inputs.input);
}

fn solve(input: &str) {
    let mut graph = Graph::parse(input);

    let part1 = graph.count_paths("you", "out");
    println!("part 1: {:?}", part1);

    let a1 = graph.count_paths("svr", "dac");
    let b1 = graph.count_paths("dac", "fft");
    let c1 = graph.count_paths("fft", "out");

    let a2 = graph.count_paths("svr", "fft");
    let b2 = graph.count_paths("fft", "dac");
    let c2 = graph.count_paths("dac", "out");

    let part2 = (a1 * b1 * c1) + (a2 * b2 * c2);
    println!("part 2: {:?}", part2);
}

#[derive(Debug)]
struct Graph<'a> {
    ids_map: HashMap<&'a str, usize>,
    nodes: Vec<Vec<usize>>,
}

impl<'a> Graph<'a> {
    fn count_paths(&mut self, from: &'a str, to: &'a str) -> usize {
        fn calc(graph: &Graph, from: usize, to: usize, cache: &mut Vec<Option<usize>>) -> usize {
            if from == to {
                return 1;
            }

            if let Some(value) = cache[from] {
                return value;
            }

            let result = graph.nodes[from]
                .iter()
                .map(|&i| calc(graph, i, to, cache))
                .sum::<usize>();
            cache[from] = Some(result);
            result
        }

        let from_id = self.get_id(from);
        let to_id = self.get_id(to);

        calc(&self, from_id, to_id, &mut vec![None; self.nodes.len()])
    }

    fn new() -> Graph<'a> {
        Graph {
            ids_map: HashMap::new(),
            nodes: vec![],
        }
    }

    fn get_id(&mut self, node: &'a str) -> usize {
        let next_id = self.ids_map.len();
        *self.ids_map.entry(node).or_insert_with(|| {
            self.nodes.push(vec![]);
            next_id
        })
    }

    fn link(&mut self, from: &'a str, to: &'a str) {
        let from_id = self.get_id(from);
        let to_id = self.get_id(to);
        self.nodes[from_id].push(to_id);
    }

    fn parse(input: &'a str) -> Graph<'a> {
        let mut graph = Graph::new();

        for line in input.trim().lines() {
            let (from, tos_str) = line.split_once(": ").unwrap();

            for to in tos_str.split_whitespace() {
                graph.link(from, to);
            }
        }

        graph
    }
}



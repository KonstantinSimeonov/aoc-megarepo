use std::{collections::HashMap, str::FromStr};

use aoc2026::Inputs;

use crate::equation::{Equation, EquationSystem};
mod equation;

fn main() {
    let inputs = Inputs::read();
    solve(&inputs.test_inputs[0]);
    solve(&inputs.input);
}

fn solve(input: &str) {
    let machines: Vec<Machine> = input
        .lines()
        .map(|line| line.parse::<Machine>().unwrap())
        .collect();

    println!("{} machines", machines.len());

    //part1(&machines);
    part2(&machines);
}

fn part2(machines: &[Machine]) {
    let part2 = machines.iter()
        .map(|m| {
            //println!("{:?}", m);
            //m.equations.print();
            //println!("====== bounds ======");
            let maxes: Vec<i64> = m.toggles.iter().map(
                |toggle|
                        toggle
                            .iter()
                            .map(|t| m.counters[*t as usize])
                            .min()
                            .unwrap()
            ).collect();

            m.equations.bounds(&maxes).unwrap()
        }).sum::<i64>();

    println!("{}", part2);
}

#[allow(dead_code)]
fn part1(machines: &[Machine]) {
    let part1: i64 = machines
        .iter()
        .map(|m| {
            m.min_toggles()
        })
        .sum();
    println!("{}", part1);
}

#[derive(Debug)]
struct Machine {
    bit_goal: i64,
    bit_toggles: Vec<i64>,
    toggles: Vec<Vec<i64>>,
    counters: Vec<i64>,
    equations: EquationSystem
}

impl Machine {
    fn min_toggles(self: &Machine) -> i64 {
        let mut results = HashMap::<i64, i64>::new();
        self.calc_min_toggles_to_goal(0, -1, 0, &mut results);
        //println!("{:?}", results);
        *results.get(&self.bit_goal).unwrap()
    }

    fn calc_min_toggles_to_goal(
        self: &Machine,
        state: i64,
        last: i64,
        depth: i64,
        results: &mut HashMap<i64, i64>,
    ) {
        if let Some(score) = results.get_mut(&state) {
            if *score <= depth {
                return
            }

            *score = depth;
        } else {
            results.insert(state, depth);
        }

        if state == self.bit_goal {
            return;
        }

        if let Some(score) = results.get(&self.bit_goal) && *score <= depth {
            return;
        }

        for i in 0..self.bit_toggles.len() {
            if last == self.bit_toggles[i] {
                continue;
            }

            self.calc_min_toggles_to_goal(
                state ^ self.bit_toggles[i],
                self.bit_toggles[i],
                depth + 1,
                results,
            );
        }
    }

    fn min_counters(self: &Machine) -> i64 {
        let mut results = HashMap::new();
        let mut state = self.counters.clone();
        self.calc_min_counters_to_goal(&mut state, 0, &mut results);

        let mut goal = self.counters.clone();
        goal.fill(0);

        *results.get(&0).unwrap()
    }

    fn calc_min_counters_to_goal(
        self: &Machine,
        state: &mut Vec<i64>,
        depth: i64,
        results: &mut HashMap<i64, i64>
    ) {
        if let Some(score) = results.get_mut(&hash(state)) {
            if *score <= depth {
                return
            }

            *score = depth;
        } else {
            results.insert(hash(state), depth);
        }

        if state.iter().all(|x| *x == 0) {
            return
        }

        if let Some(score) = results.get(&0) && *score <= depth {
            return;
        }

        for i in 0..self.toggles.len() {
            let opt = &self.toggles[i];
            let mut bad = false;
            for j in 0..opt.len() {
                state[opt[j] as usize] -= 1;
                if state[opt[j] as usize] < 0 {
                    bad = true;
                }
            }

            if !bad {
                self.calc_min_counters_to_goal(state, depth + 1, results);
            }

            for j in 0..opt.len() {
                state[opt[j] as usize] += 1;
            }
        }
    }
}

fn hash(xs: &Vec<i64>) -> i64 {
    let mut h: i64 = 0;
    let d: i64 = 1000;
    for i in 0..xs.len() {
        h += d.pow(i as u32) * xs[i];
    }

    h
}

impl FromStr for Machine {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (goal_str, rest) = s.split_once(' ').unwrap();

        let bit_goal = strip_brackets(goal_str)
            .chars()
            .zip(0..)
            .filter(|(c, _)| *c == '#')
            .fold(0, |n, (_, i)| set_bit(n, i));

        let toggles: Vec<Vec<i64>> = rest
            .split_ascii_whitespace()
            .filter(|x| x.starts_with("("))
            .map(strip_brackets)
            .map(|toggle_str| toggle_str.split(',').map(|n| n.parse::<i64>().unwrap()).collect())
            .collect();

        let bit_toggles = toggles.iter().map(|bits_list| {
                bits_list
                    .iter()
                    .fold(0, |n, d| set_bit(n, *d))
            })
            .collect();

        let result = rest.split_once(" {").unwrap();
        let counters_str = &result.1[..result.1.len() - 1];
        let counters: Vec<i64> = counters_str.split(',').map(
            |n| n.parse::<i64>().unwrap()
        ).collect();

        let mut equations: Vec<Equation> = vec![];
        for i in 0..counters.len() {
            let mut variables = vec![];
            for toggle in toggles.iter() {
                let n = if toggle.iter().any(|x| *x == i as i64) { 1 } else { 0 };
                variables.push(n);
            }

            equations.push(Equation { variables, constant: -counters[i], variable_result: None })
        }



        Ok(Machine { bit_goal, bit_toggles, toggles, counters, equations: EquationSystem { equations } })
    }
}

fn set_bit(n: i64, bit: i64) -> i64 {
    n | (1 << bit)
}

fn strip_brackets(victim: &str) -> &str {
    &victim[1..victim.len() - 1]
}

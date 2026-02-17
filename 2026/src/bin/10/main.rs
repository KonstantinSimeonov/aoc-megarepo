use aoc2026::Inputs;

use crate::equation::{Equation, EquationSystem};
mod equation;

fn main() {
    let inputs = Inputs::read();
    println!("===== input 0 =====");
    solve(&inputs.test_inputs[0]);
    println!("===== input =====");
    solve(&inputs.input);
}

fn solve(input: &str) {
    let machines: Vec<Machine> = input
        .lines()
        .map(|line| line.parse::<Machine>().unwrap())
        .collect();

    println!("{} machines", machines.len());

    let (part1, part2) = machines.iter().fold(
        (0, 0),
        |(part1, part2), m| (part1 + m.min_toggles(), part2 + m.min_toggles_to_counters())
    );

    println!("part 1: {}, part 2: {}", part1, part2);
}

#[derive(Debug)]
struct Machine {
    // part 1
    bit_goal: i64,
    bit_toggles: Vec<i64>,

    // part 2
    toggles: Vec<Vec<i64>>,
    counters: Vec<i64>,
    equations: EquationSystem,
}

impl Machine {
    pub fn min_toggles(&self) -> i64 {
        let n = self.bit_toggles.len();
        let mut min = i64::MAX;
        for combination in 0..(1 << n) {
            let mut state = 0;
            let mut count = 0;
            for i in 0..n {
                if combination & (1 << i) != 0 {
                    state ^= self.bit_toggles[i];
                    count += 1;
                }
            }

            if state == self.bit_goal {
                min = min.min(count);
            }
        }

        min
    }

    pub fn min_toggles_to_counters(&self) -> i64 {
        let maxes: Vec<i64> = self
            .toggles
            .iter()
            .map(|toggle| {
                toggle
                    .iter()
                    .map(|t| self.counters[*t as usize])
                    .min()
                    .unwrap()
            })
            .collect();

        self.equations.minimize(&maxes).unwrap()
    }
}

impl std::str::FromStr for Machine {
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
            .map(|toggle_str| {
                toggle_str
                    .split(',')
                    .map(|n| n.parse::<i64>().unwrap())
                    .collect()
            })
            .collect();

        let bit_toggles = toggles
            .iter()
            .map(|bits_list| bits_list.iter().fold(0, |n, d| set_bit(n, *d)))
            .collect();

        let result = rest.split_once(" {").unwrap();
        let counters_str = &result.1[..result.1.len() - 1];
        let counters: Vec<i64> = counters_str
            .split(',')
            .map(|n| n.parse::<i64>().unwrap())
            .collect();

        let mut equations: Vec<Equation> = vec![];
        for i in 0..counters.len() {
            let mut variables = vec![];
            for toggle in toggles.iter() {
                let n = if toggle.iter().any(|x| *x == i as i64) {
                    1
                } else {
                    0
                };
                variables.push(n);
            }

            equations.push(Equation {
                variables,
                constant: -counters[i],
                variable_result: None,
            })
        }

        Ok(Machine {
            bit_goal,
            bit_toggles,
            toggles,
            counters,
            equations: EquationSystem { equations },
        })
    }
}

fn set_bit(n: i64, bit: i64) -> i64 {
    n | (1 << bit)
}

fn strip_brackets(victim: &str) -> &str {
    &victim[1..victim.len() - 1]
}

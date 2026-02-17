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

    let (part1, part2) = machines.iter().fold((0, 0), |(part1, part2), m| {
        (part1 + m.min_toggles(), part2 + m.min_toggles_to_counters())
    });

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
        let parts = s.split_whitespace().collect::<Vec<&str>>();

        let bit_goal = strip_brackets(parts[0])
            .chars()
            .zip(0..)
            .filter(|(c, _)| *c == '#')
            .fold(0, |n, (_, i)| set_bit(n, i));

        let toggles: Vec<Vec<i64>> = parts[1..parts.len() - 1]
            .iter()
            .map(|toggle_str| {
                strip_brackets(toggle_str)
                    .split(',')
                    .map(|n| n.parse::<i64>().unwrap())
                    .collect()
            })
            .collect();

        let bit_toggles: Vec<i64> = toggles
            .iter()
            .map(|bits_list| bits_list.iter().fold(0, |n, d| set_bit(n, *d)))
            .collect();

        let counters: Vec<i64> = strip_brackets(parts[parts.len() - 1])
            .split(',')
            .map(|n| n.parse::<i64>().unwrap())
            .collect();

        let equations: Vec<Equation> = counters
            .iter()
            .enumerate()
            .map(|(i, counter)| {
                let coefficients = bit_toggles
                    .iter()
                    .map(|toggle| (toggle & (1 << i)) >> i)
                    .collect();

                Equation {
                    coefficients,
                    constant: -counter,
                    variable_result: None,
                }
            })
            .collect();

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

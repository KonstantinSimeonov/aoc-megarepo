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

    // The each counter provides a linear equation:
    //
    // (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7} becomes
    // a   b     c   d     e     f
    //
    // e * (0, 2) + f * (0, 1) = 3
    // b * (1, 3) + f * (0, 1) = 5
    // c * (2) + d * (2, 3) + e * (0, 2) = 4
    // a * (3) + b * (1, 3) + d * (2, 3) = 7
    //
    // each of the variables a, b, c, d, e, f represents
    // how many times we clicked some buttons. The buttons
    // are not actually relevent for those equations after
    // we parse them initially, so the equations can be written as
    // e + f = 3
    // b + f = 5
    // c + d + e = 4
    // a + b + d = 7
    //
    // Our answer is the minimum of a + b + c + d + e + f where
    // all of the equations above are still fullfilled.
    equations: EquationSystem,
}

impl Machine {
    // since toggling on/off is just like XOR
    // it doesn't matter how exactly many times the buttons
    // are pressed, just whether they're pressed an even
    // or odd number of times which for the case of XOR
    // is the same as 0 or 1 times.
    //
    // to make fast, run over all possible combinations
    // of the toggles where they're either picked or not picked
    // and track the state they lead to by XORing them.
    // At the end, if the state matches the goal, this
    // configuration is a candidate.
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

    // since looping over the value combinations of all variables would take ages,
    // we can see what variables can be expressed as other
    // variables and look just run over the value combinations for variables that
    // can't be expressed as other variables. This can be done through substitution:
    //
    // e + f = 3
    // b + f = 5
    // c + d + e = 4
    // a + b + d = 7
    //
    // e = 3 - f
    // b = 5 - f
    // c = 4 - d - e = 4 - d - (3 - f)
    // a = 7 - b - d = 7 - (5 - f) - d
    //
    // Result:
    // e = 3 - f
    // b = 5 - f
    // c = 1 - d + f
    // a = 2 + f - d
    //
    // Above, all variables have been expressed through f and d, so we need to
    // loop over value combinations only for f and d.
    //
    // Consider the limits for f and d
    // (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7} becomes
    // a   b     c   d     e     f
    //
    // f is in [0; min(3, 5)]
    // d is in [0; min(4, 7)]
    // because that's the most times a button can be pressed.
    //
    // Looping over those values for f and d, we can then evaluate all the equations,
    // since every variable is expressed through f and f
    // and discard results that are negative as invalid, since a button can't
    // be pressed a negative number of times.
    //
    // If the result of the equation is valid, then we take a + b + c + d + e + f
    // as a candidate for a minimum.
    pub fn min_toggles_to_counters(&self) -> i64 {
        // a list of maxes (pre var maxes) compared to a single max(counters)
        // is just an optimization to have to run through less cases
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

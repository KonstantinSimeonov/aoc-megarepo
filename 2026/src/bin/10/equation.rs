#[derive(Debug, Clone)]
pub struct EquationSystem {
    pub equations: Vec<Equation>,
}

impl EquationSystem {
    pub fn substite(&self) -> EquationSystem {
        let mut equations = self.equations.clone();

        for i in 0..equations.len() {
            let eq = &equations[i];
            let first_var_index = eq.first_index();
            if first_var_index.is_none() {
                continue;
            }

            equations[i] = eq.solve_for(first_var_index.unwrap());
            for j in i + 1..equations.len() {
                equations[j] = equations[j].substitute(&equations[i]);
            }
        }

        EquationSystem { equations }
    }

    pub fn minimize(&self, uppers_bounds: &[i64]) -> Option<i64> {
        let solved = self.substite();
        let solved_vars: Vec<VarId> = solved
            .equations
            .iter()
            .filter_map(|eq| eq.variable_result)
            .collect();
        let total_vars = self.var_count();
        let free_vars: Vec<VarId> = (0..total_vars)
            .filter(|i| !solved_vars.contains(i))
            .collect();

        let maxes: Vec<i64> = free_vars
            .iter()
            .map(|var_index| uppers_bounds[*var_index])
            .collect();

        solved.minimize_solved(&free_vars, &mut vec![0; total_vars], &maxes)
    }

    pub fn var_count(&self) -> usize {
        self.equations[0].coefficients.len()
    }

    pub fn minimize_solved(
        &self,
        free: &[VarId],
        values: &mut Vec<i64>,
        maxes: &[i64],
    ) -> Option<i64> {
        if free.is_empty() {
            return self.eval(values);
        }

        let var = free[0];
        (0..=maxes[0])
            .filter_map(|v| {
                values[var] = v;
                self.minimize_solved(&free[1..], values, &maxes[1..])
            })
            .min()
    }

    pub fn eval(&self, variable_values: &Vec<i64>) -> Option<i64> {
        let mut values: Vec<i64> = variable_values.clone();

        for eq in self.equations.iter().rev() {
            if let Some(var_index) = eq.variable_result {
                let coeff = eq.coefficients[var_index];
                // values[var_index] is 0, so this is everything else
                let rest = eq.eval(&values);

                // non-integer button presses, discard it
                if rest % coeff != 0 {
                    return None;
                }

                let var_value = -rest / coeff;
                // negative solution button presses, discard
                if var_value < 0 {
                    return None;
                }

                values[var_index] = var_value;
            }
        }

        Some(values.iter().sum())
    }

    #[allow(dead_code)]
    pub fn print(&self) {
        println!("System of {} equations:", self.equations.len());
        for e in self.equations.iter() {
            e.print();
        }
    }
}

#[derive(Debug, Clone)]
pub struct Equation {
    pub coefficients: Vec<i64>,
    pub constant: i64,
    pub variable_result: Option<VarId>,
}

impl Equation {
    pub fn indexes(&self) -> impl Iterator<Item = usize> {
        self.coefficients
            .iter()
            .enumerate()
            .filter(|(_, coeff)| **coeff != 0)
            .map(|(i, _)| i)
    }

    pub fn first_index(&self) -> Option<usize> {
        self.indexes().next()
    }

    pub fn solve_for(&self, var_index: VarId) -> Equation {
        Equation {
            coefficients: self.coefficients.clone(),
            constant: self.constant,
            variable_result: Some(var_index),
        }
    }

    pub fn substitute(&self, other: &Equation) -> Equation {
        let var_index = other.variable_result.unwrap();
        let a = other.coefficients[var_index];
        let b = self.coefficients[var_index];

        // the current equation isn't expressed in terms of the
        // other equation. nothing to do
        if b == 0 {
            return self.clone();
        }

        let coefficients = self
            .coefficients
            .iter()
            .zip(other.coefficients.iter())
            // do this multiplication with a and b to avoid
            // having to divide and use floating point numbers, which
            // opens a very annoying can of worms that I don't know how to
            // deal with, because it yields non-integer results, like
            // pressing a button 2.5 times.
            //
            // example:
            // 2f = d + c + 3
            // e = 3f - c + 4
            //
            // substitution of "2f = d + c + 3" into "e = 3f - c + 4") is a bit hairy,
            // since we'd have to divide to get f and plug it into 3f. Instead, multiply
            // the eq for f by 3 and the eq for e by 2 to get them both to be 6.
            //
            // 2e = 3(3(d + c + 3)) - 2(c + 4)
            //
            // For this example, a=2 and b=3.
            .map(|(s, o)| a * s - b * o)
            .collect();

        let constant = a * self.constant - b * other.constant;

        Equation {
            coefficients,
            constant,
            variable_result: None,
        }
    }

    pub fn eval(&self, variable_values: &[i64]) -> i64 {
        self.constant
            + self
                .coefficients
                .iter()
                .zip(variable_values.iter())
                .map(|(coeff, value)| coeff * value)
                .sum::<i64>()
    }

    #[allow(dead_code)]
    pub fn print(&self) {
        println!(
            "{:?} + {} = {:?}",
            self.coefficients, self.constant, self.variable_result
        );
        println!("{}", self);
        println!()
    }
}

impl std::fmt::Display for Equation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lolphabet = "abcdefghijklmnopqrstuvwzyz";
        let mut parts: Vec<String> = vec![];
        for i in 0..self.coefficients.len() {
            let coeff = self.coefficients[i];
            if coeff == 0 {
                continue;
            }

            if coeff < 0 && parts.len() > 0 {
                parts.push("-".to_string());
            } else if parts.len() > 0 {
                parts.push("+".to_string())
            }

            let letter = &lolphabet[i..i + 1];
            if coeff.abs() != 1 {
                parts.push(format!("{}{}", coeff.abs().to_string(), letter));
            } else {
                parts.push(letter.to_string());
            }
        }

        let constant = match self.constant {
            0 => "".to_string(),
            x if x > 0 => format!("+ {}", x),
            x => format!("- {}", x.abs()),
        };

        let result = format!(
            "{} {} = {}",
            parts.join(" "),
            constant,
            self.variable_result
                .map(|i| &lolphabet[i..i + 1])
                .unwrap_or("0")
        );

        f.write_str(&result)
    }
}

type VarId = usize;

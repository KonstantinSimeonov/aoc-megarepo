#[derive(Debug, Clone)]
pub struct EquationSystem {
    pub equations: Vec<Equation>,
}

impl EquationSystem {
    pub fn print(&self) {
        println!("System of {} equations:", self.equations.len());
        for e in self.equations.iter() {
            e.print();
        }
    }

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
                equations[j] = equations[j].substite(&equations[i]);
            }
        }

        EquationSystem { equations }
    }

    pub fn bounds(&self, uppers: &Vec<i64>) -> Option<i64> {
        let solved = self.substite();
        let solved_vars: Vec<usize> = solved.equations.iter().filter_map(|eq| eq.variable_result).collect();
        let total_vars = self.var_count();
        let free_vars: Vec<usize> = (0..total_vars)
            .filter(|i| !solved_vars.contains(i))
            .collect();

        let maxes: Vec<i64> = free_vars.iter().map(|var_index| uppers[*var_index]).collect();
        let min = solved.minimize(&free_vars, &mut vec![0; total_vars], &maxes);

        min
    }

    pub fn var_count(&self) -> usize {
        self.equations[0].variables.len()
    }

    pub fn minimize(&self, free: &[usize], values: &mut Vec<i64>, maxes: &[i64]) -> Option<i64> {
        if free.is_empty() {
            return self.eval(values)
        }

        let var = free[0];
        (0..=maxes[0]).filter_map(
            |v| {
                values[var] = v;
                self.minimize(&free[1..], values, &maxes[1..])
            }
        ).min()
    }

    pub fn eval(&self, vars: &Vec<i64>) -> Option<i64> {
        //println!("eval:");
        let mut variables: Vec<i64> = vars.clone();

        for eq in self.equations.iter().rev() {
            if let Some(var_index) = eq.variable_result {
                let coeff = eq.variables[var_index];
                // variables[var_index] is 0, so this is everything else
                let rest = eq.eval(&variables);

                if rest % coeff != 0 {
                    return None
                }

                let var_value = -rest / coeff;
                if var_value < 0 {
                    return None
                }

                //println!("    evaluated {} to {} (var_index={})", eq, var_value, var_index);
                variables[var_index] = var_value;
            }
        }

        Some(variables.iter().sum())
    }

}

#[derive(Debug, Clone)]
pub struct Equation {
    pub variables: Vec<i64>,
    pub constant: i64,
    pub variable_result: Option<usize>,
}

impl std::fmt::Display for Equation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lolphabet = "abcdefghijklmnopqrstuvwzyz";
        let mut parts: Vec<String> = vec![];
        for i in 0..self.variables.len() {
            let coeff = self.variables[i];
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

impl Equation {
    pub fn indexes(&self) -> impl Iterator<Item = usize> {
        (0..)
            .zip(self.variables.iter())
            .filter(|(_, coeff)| **coeff != 0)
            .map(|(i, _)| i)
    }

    pub fn first_index(&self) -> Option<usize> {
        self.indexes().next()
    }

    pub fn solve_for(&self, var_index: usize) -> Equation {
        Equation {
            variables: self.variables.clone(),
            constant: self.constant,
            variable_result: Some(var_index),
        }
    }

    pub fn substite(&self, other: &Equation) -> Equation {
        let var_index = other.variable_result.unwrap();
        let a = other.variables[var_index];
        let b = self.variables[var_index];

        if b == 0 {
            return self.clone();
        }

        let variables = self.variables.iter()
            .zip(other.variables.iter())
            .map(|(s, o)| a * s - b * o)
            .collect();

        let constant = a * self.constant - b * other.constant;

        Equation {
            variables,
            constant,
            variable_result: None,
        }
    }

    pub fn eval(&self, variable_values: &[i64]) -> i64 {
        self.constant
            + self
                .variables
                .iter()
                .zip(variable_values.iter())
                .map(|(coeff, value)| coeff * value)
                .sum::<i64>()
    }

    pub fn print(&self) {
        println!(
            "{:?} + {} = {:?}",
            self.variables, self.constant, self.variable_result
        );
        println!("{}", self);
        println!()
    }
}

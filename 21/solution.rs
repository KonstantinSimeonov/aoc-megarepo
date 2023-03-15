use std::collections::HashMap;
use std::fs;

fn main() {
    let input = fs::read_to_string("./input").expect("ez clap");

    let funcs = input
        .trim()
        .lines()
        .map(|line| {
            let f = Func::new(line);
            match f {
                Func::Const(name, _) => (name, f),
                Func::Fn(name, _, _, _) => (name, f),
            }
        })
        .collect::<HashMap<_, _>>();

    let root = funcs.get("root").unwrap();

    let part1 = root.eval(&funcs);
    println!("part 1: {}", part1);

    let mut br = root.find_branch("humn", &funcs).unwrap();
    br.pop();

    let mut x = -root.reverse(0, br[br.len() - 1], &funcs);

    while let Some(func) = br.pop() {
        if br.len() == 0 {
            break;
        }

        let arg = *br.last().unwrap();
        x = funcs.get(func).unwrap().reverse(x, arg, &funcs);
    }

    println!("part 2: {}", x)
}

#[derive(Debug)]
enum Func<'a> {
    Fn(&'a str, &'a str, &'a str, &'a str),
    Const(&'a str, i64),
}

impl<'a> Func<'_> {
    fn eval(&self, fns: &HashMap<&'a str, Func>) -> i64 {
        match self {
            Func::Const(_, x) => *x,
            Func::Fn(_, a1, a2, op) => {
                let [v1, v2] = [a1, a2].map(|a| fns.get(a).unwrap().eval(fns));
                match op {
                    &"*" => v1 * v2,
                    &"-" => v1 - v2,
                    &"+" => v1 + v2,
                    &"/" => v1 / v2,
                    _ => unreachable!(),
                }
            }
        }
    }

    fn new(s: &'a str) -> Func {
        let (name, rest) = s.split_once(": ").unwrap();
        let body = rest.split(' ').collect::<Vec<_>>();

        match &body[..] {
            [a1, op, a2] => Func::Fn(name, a1, a2, op),
            [val] => Func::Const(name, val.parse::<i64>().unwrap()),
            _ => unreachable!(),
        }
    }

    fn find_branch(
        &'a self,
        func: &'a str,
        funcs: &'a HashMap<&'a str, Func>,
    ) -> Option<Vec<&'a str>> {
        match self {
            Func::Const(name, _) => {
                if *name == func {
                    Some(Vec::from([func]))
                } else {
                    None
                }
            }
            Func::Fn(name, a1, a2, _) => {
                let res = [a1, a2]
                    .iter()
                    .find_map(|f| funcs.get(*f).and_then(|f1| f1.find_branch(func, funcs)));

                res.map(|mut path| {
                    path.push(name);
                    path
                })
            }
        }
    }

    fn reverse(&self, x: i64, arg: &'a str, funcs: &HashMap<&'a str, Func>) -> i64 {
        match self {
            Func::Fn(_, a1, a2, op) => {
                let f1 = funcs.get(a1).unwrap();
                let f2 = funcs.get(a2).unwrap();
                let other = if arg == *a1 { f2 } else { f1 };
                match op {
                    &"+" => x - other.eval(funcs),

                    &"*" => x / other.eval(funcs),

                    &"-" if arg == *a1 => x + f2.eval(funcs),
                    &"-" => f1.eval(funcs) - x,

                    &"/" if arg == *a1 => x * f2.eval(funcs),
                    &"/" => f1.eval(funcs) / x,
                    _ => unreachable!(),
                }
            }
            Func::Const(name, _) if *name == arg => x,
            _ => unreachable!(),
        }
    }
}

use std::{collections::HashSet, fs};

fn incr(d: i32) -> bool {
    -3 <= d && d < 0
}

fn decr(d: i32) -> bool {
    0 < d && d <= 3
}

fn diff_inplace(row: &mut Vec<i32>) {
    for i in 0..row.len() - 1 {
        row[i] = row[i] - row[i + 1];
    }

    row.pop();
}

#[derive(PartialEq, Eq, Debug)]
enum IsSafe {
    Safe,
    SafeWithDampener,
    Unsafe,
}

fn is_safe(diffs: &Vec<i32>, pred: fn(i32) -> bool) -> IsSafe {
    let mut used_dampener = false;

    let mut i = 0;

    while i < diffs.len() {
        if pred(diffs[i]) {
            i += 1;
            continue;
        }

        if used_dampener {
            return IsSafe::Unsafe;
        }
        used_dampener = true;

        if i < diffs.len() - 1 && pred(diffs[i] + diffs[i + 1]) {
            i += 2;
            continue;
        }

        if i > 0 && pred(diffs[i - 1] + diffs[i]) {
            i += 1;
            continue;
        }

        if i == 0 || i == diffs.len() - 1 {
            i += 1;
            continue;
        }

        return IsSafe::Unsafe;
    }

    if used_dampener {
        IsSafe::SafeWithDampener
    } else {
        IsSafe::Safe
    }
}

fn parse<'a>(str: &'a str) -> impl Iterator<Item = Vec<i32>> + use<'a> {
    str.trim().split("\n").map(|line| {
        line.split(' ')
            .map(|num| num.parse::<i32>().unwrap())
            .collect::<Vec<_>>()
    })
}

fn solve(str: &str) -> (usize, usize) {
    let rows = parse(str);
    let mut a1 = 0;
    let mut a2 = 0;
    for mut row in rows {
        diff_inplace(&mut row);
        let verdict = match is_safe(&row, incr) {
            IsSafe::Unsafe => is_safe(&row, decr),
            x => x,
        };

        if verdict != IsSafe::Unsafe {
            a2 += 1;
        }

        if verdict == IsSafe::Safe {
            a1 += 1;
        }
    }

    (a1, a2)
}

fn is_safe_dumb(xs: &[i32]) -> bool {
  let diffs = 
    xs.iter()
        .zip(xs.iter().skip(1))
        .map(|(a, b)| a - b)
        .collect::<HashSet<i32>>();

  diffs.is_subset(&HashSet::from([1, 2, 3])) || diffs.is_subset(&HashSet::from([-1, -2, -3]))
}

fn solve_brute(str: &str) -> (usize, usize) {
    let rows = parse(str);

    let mut a1 = 0;
    let mut a2 = 0;

    for row in rows {
        let safe = is_safe_dumb(&row);
        let safe_with_dampen = safe || (0..row.len()).any(|i| {
            let mut x = row.clone();
            x.remove(i);
            is_safe_dumb(&x)
        });

        if safe_with_dampen {
          a2 += 1;
        }

        if safe {
          a1 += 1;
        }
    };

    (a1, a2)
}

fn main() {
    let input0 = fs::read_to_string("./02/input0").unwrap();
    let input = fs::read_to_string("./02/input").unwrap();
    println!("{:?}", solve(&input0));
    println!("{:?}", solve(&input));
    println!("{:?}", solve_brute(&input0));
    println!("{:?}", solve_brute(&input));
}

// (x - y)
// (y - z)
// x - z

#[cfg(test)]
mod tests {
    use super::*;

    fn t(mut input: Vec<i32>, pred: fn(i32) -> bool, expected: IsSafe) {
        diff_inplace(&mut input);
        assert_eq!(is_safe(&input, pred), expected);
    }

    #[test]
    fn kek() {
      let input = fs::read_to_string("./input").unwrap();
      assert_eq!(solve(&input), solve_brute(&input))
    }

    #[test]
    fn solution() {
        let input = fs::read_to_string("./input").unwrap();
        assert_eq!(solve(&input), (402, 455))
    }

    #[test]
    fn stuff() {
        t(vec![7, 6, 4, 2, 1], incr, IsSafe::Unsafe);
        t(vec![1, 2, 7, 8, 9], incr, IsSafe::Unsafe);
        t(vec![9, 7, 6, 2, 1], incr, IsSafe::Unsafe);
        t(vec![1, 3, 2, 4, 5], incr, IsSafe::SafeWithDampener);
        t(vec![8, 6, 4, 4, 1], incr, IsSafe::Unsafe);
        t(vec![1, 3, 6, 7, 9], incr, IsSafe::Safe);

        t(vec![7, 6, 4, 2, 1], decr, IsSafe::Safe);
        t(vec![1, 2, 7, 8, 9], decr, IsSafe::Unsafe);
        t(vec![9, 7, 6, 2, 1], decr, IsSafe::Unsafe);
        t(vec![1, 3, 2, 4, 5], decr, IsSafe::Unsafe);
        t(vec![8, 6, 4, 4, 1], decr, IsSafe::SafeWithDampener);
        t(vec![1, 3, 6, 7, 9], decr, IsSafe::Unsafe);

        t(vec![10, 1, 2, 3], incr, IsSafe::SafeWithDampener);
        t(vec![-10, 10, 9, 8], decr, IsSafe::SafeWithDampener);
        t(vec![1, 2, 3, 10], incr, IsSafe::SafeWithDampener);
        t(
            vec![89, 92, 95, 93, 94, 97, 98],
            incr,
            IsSafe::SafeWithDampener,
        );
        t(vec![6, 7, 5, 4, 2, 2], decr, IsSafe::Unsafe);
        t(vec![6, 7, 5, 4, 2, 1], decr, IsSafe::SafeWithDampener);
        t(vec![6, -7, 5, 4, 2, 1], decr, IsSafe::SafeWithDampener);
    }
}

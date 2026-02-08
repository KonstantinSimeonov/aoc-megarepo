use std::cmp;

use aoc2026::Inputs;

fn main() {
    let inputs = Inputs::read();
    solve(&inputs.test_inputs[0]);
    solve(&inputs.input);
}

fn solve(input: &str) {
    let points: Vec<Point2D> = input
        .lines()
        .map(|line| {
            let mut it = line.split(',').map(|x| x.parse::<i64>().unwrap());
            (it.next().unwrap(), it.next().unwrap())
        })
        .collect();

    let len = points.len();

    let rects: Vec<Rect> = (0..len - 1)
        .flat_map(|i| (i + 1..len).map(move |j| (i, j)))
        .map(|(i, j)| Rect {
            a: points[i],
            b: points[j],
        })
        .collect();

    let part1 = rects.iter().map(|r| r.area()).max();

    let part2 = rects
        .iter()
        .filter(|&r| {
            for k in 0..len {
                let a = points[k];
                let b = points[(k + 1) % len];
                if r.intersects(&Rect { a, b }) {
                    return false;
                }
            }

            return true;
        })
        .map(|r| r.area())
        .max();

    println!("{:?}", part1);
    println!("{:?}", part2);
}

struct Rect {
    a: Point2D,
    b: Point2D,
}

impl Rect {
    fn area(self: &Rect) -> i64 {
        let x = (self.a.0 - self.b.0).abs() + 1;
        let y = (self.a.1 - self.b.1).abs() + 1;
        x * y
    }

    fn intersects(self: &Rect, other: &Rect) -> bool {
        let (sx_min, sx_max) = min_max(self.a.0, self.b.0);
        let (sy_min, sy_max) = min_max(self.a.1, self.b.1);
        let (ox_min, ox_max) = min_max(other.a.0, other.b.0);
        let (oy_min, oy_max) = min_max(other.a.1, other.b.1);

        sx_min < ox_max && ox_min < sx_max && sy_min < oy_max && oy_min < sy_max
    }
}

type Point2D = (i64, i64);

fn min_max<T: cmp::Ord>(a: T, b: T) -> (T, T) {
    if a > b {
        (b, a)
    } else {
        (a, b)
    }
}

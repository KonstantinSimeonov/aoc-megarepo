use std::collections::HashSet;
use std::fs;

type Point = (i32, i32);

fn parse(input: &str) -> (Vec<(char, Point)>, i32, i32) {
    let mut antennas = vec![];
    let mut rows = 0;
    let mut cols = 0;

    for (line, r) in input.trim().lines().zip(0..) {
        for (ch, c) in line.chars().zip(0..) {
            if ch != '.' {
                antennas.push((ch, (r, c)));
            }

            cols = c;
        }

        rows = r;
    }

    (antennas, rows + 1, cols + 1)
}

fn is_inside(r: i32, c: i32, rows: i32, cols: i32) -> bool {
    0 <= r && r < rows && 0 <= c && c < cols
}

fn cross(
    x: &Point,
    y: &Point,
    rows: i32,
    cols: i32,
    v1: &mut HashSet<Point>,
    v2: &mut HashSet<Point>,
) {
    let dr = x.0 - y.0;
    let dc = x.1 - y.1;

    let mut r = x.0 + dr;
    let mut c = x.1 + dc;

    if is_inside(r, c, rows, cols) {
        v1.insert((r, c));
    }

    v2.insert(x.clone());
    while is_inside(r, c, rows, cols) {
        v2.insert((r, c));
        r += dr;
        c += dc;
    }
}

fn solve(input: &str) -> (usize, usize) {
    let (antennas, rows, cols) = parse(input);

    let mut v1 = HashSet::new();
    let mut v2 = HashSet::new();

    for i in 0..antennas.len() {
        for j in (i + 1)..antennas.len() {
            let (c1, x) = &antennas[i];
            let (c2, y) = &antennas[j];

            if c1 == c2 {
                cross(x, y, rows, cols, &mut v1, &mut v2);
                cross(y, x, rows, cols, &mut v1, &mut v2);
            }
        }
    }

    let a1 = v1.len();
    let a2 = v2.len();

    //p(&mut v2, rows, cols);
    (a1, a2)
}

fn main() {
    let i0 = fs::read_to_string("./08/input0").unwrap();
    println!("{:?}", solve(&i0));

    let i3 = fs::read_to_string("./08/input3").unwrap();
    println!("{:?}", solve(&i3));

    let i = fs::read_to_string("./08/input").unwrap();
    println!("{:?}", solve(&i));
}

fn p(ans: &mut HashSet<Point>, rows: i32, cols: i32) {
    for i in 0..rows {
        for j in 0..cols {
            let c = if ans.contains(&(i, j)) {
                ans.remove(&(i, j));
                'x'
            } else {
                '.'
            };
            print!("{}", c);
        }

        println!()
    }

    println!("{:?}", &ans);
}

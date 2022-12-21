use std::{collections::HashSet, fs};

fn main() {
    let input = fs::read_to_string("./input").expect("cubes");

    let cubes = input
        .trim()
        .split('\n')
        .map(|l| {
            let xyz = l
                .split(',')
                .map(|x| x.parse::<i32>().unwrap())
                .collect::<Vec<_>>();

            match xyz[..] {
                [x, y, z] => (x, y, z),
                _ => unreachable!(),
            }
        })
        .collect::<HashSet<_>>();

    let deltas = [
        (1, 0, 0),
        (-1, 0, 0),
        (0, 1, 0),
        (0, -1, 0),
        (0, 0, 1),
        (0, 0, -1),
    ];

    let res = cubes.iter().fold(0, |sum, &(cx, cy, cz)| {
        sum + deltas.iter().fold(0, |cs, &(dx, dy, dz)| {
            cs + (!cubes.contains(&(cx + dx, cy + dy, cz + dz))) as usize
        })
    });

    println!("{:?}", res)
}

use std::{collections::HashSet, fs, cmp};

fn add3(x: &(i32, i32, i32), y: &(i32, i32, i32)) -> (i32, i32, i32) {
    (x.0 + y.0, x.1 + y.1, x.2 + y.2)
}

fn is_inside(&(x, y, z): &(i32, i32, i32), min: i32, max: i32) -> bool {
    [x, y, z].iter().all(|&c| min <= c && c <= max)
}

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

    let total_surface = cubes.iter().fold(0, |sum, p| {
        sum + deltas.iter().fold(0, |cs, delta| {
            cs + (!cubes.contains(&add3(p, delta))) as usize
        })
    });

    println!("part 1: {}", total_surface);

    let bounds = cubes
        .iter()
        .flat_map(|(x, y, z)| [x, y, z])
        .fold((i32::MAX, i32::MIN), |(min, max), &x| (cmp::min(min, x), cmp::max(max, x)));

    let min = bounds.0 - 1;
    let max = bounds.1 + 1;

    let mut outside_surface = 0;
    let mut cells = Vec::from([(min, min, min)]);
    let mut visited = HashSet::new();

    while let Some(p) = cells.pop() {
        let next = deltas
            .iter()
            .map(|delta| add3(&p, delta))
            .filter(|p| is_inside(p, min, max))
            .filter(|p| {
                if visited.contains(p) {
                    return false
                }

                if cubes.contains(p) {
                    outside_surface += 1;
                    return false
                }

                visited.insert(*p);

                return true
            });

        cells.extend(next);
    }

    println!("part 2: {}", outside_surface)
}

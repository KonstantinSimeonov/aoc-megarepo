use std::fs;
use std::collections::HashSet;

fn main() {
    let input = fs::read_to_string("./input")
        .expect("da stuff to happin");

    let res = input
        .lines()
        .map(|line| line.chars().collect::<Vec<char>>())
        .collect::<Vec<Vec<char>>>();

    let always_vis = (res.len() + (res[0].len() - 2)) * 2;

    let mut hor_viz = HashSet::new();

    for i in 1..res.len() - 1 {
        let mut max_left = res[i][0];
        let mut max_right = res[i][res.len() - 1];
        for j in 1..res[i].len() - 1 {
            if res[i][j] > max_left {
                hor_viz.insert((i, j));
                max_left = res[i][j];
            }

            let rj = res.len() - j - 1;
            if res[i][rj] > max_right {
                hor_viz.insert((i, rj));
                max_right = res[i][rj];
            }
        }
    }

    let mut vert_viz = HashSet::new();
    for j in 1..res[0].len() - 1 {
        let mut max_top = res[0][j];
        let mut max_bottom = res[res.len() - 1][j];
        for i in 1..res[j].len() - 1 {
            if res[i][j] > max_top {
                vert_viz.insert((i, j));
                max_top = res[i][j];
            }

            let bi = res.len() - i - 1;
            if res[bi][j] > max_bottom {
                vert_viz.insert((bi, j));
                max_bottom = res[bi][j];
            }
        }
    }

    let p1 = hor_viz.union(&vert_viz).count() + always_vis;
    println!("part1: {}", p1)
}

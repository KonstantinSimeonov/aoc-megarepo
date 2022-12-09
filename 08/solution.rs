use std::fs;
use std::collections::HashSet;

fn main() {
    let input = fs::read_to_string("./input")
        .expect("da stuff to happin");

    let trees = input
        .lines()
        .map(|line| line.chars().collect::<Vec<char>>())
        .collect::<Vec<Vec<char>>>();

    let always_vis = (trees.len() + (trees[0].len() - 2)) * 2;

    let mut hor_viz = HashSet::new();

    for i in 1..trees.len() - 1 {
        let mut max_left = trees[i][0];
        let mut max_right = trees[i][trees.len() - 1];
        for j in 1..trees[i].len() - 1 {
            if trees[i][j] > max_left {
                hor_viz.insert((i, j));
                max_left = trees[i][j];
            }

            let rj = trees.len() - j - 1;
            if trees[i][rj] > max_right {
                hor_viz.insert((i, rj));
                max_right = trees[i][rj];
            }
        }
    }

    let mut vert_viz = HashSet::new();
    for j in 1..trees[0].len() - 1 {
        let mut max_top = trees[0][j];
        let mut max_bottom = trees[trees.len() - 1][j];
        for i in 1..trees[j].len() - 1 {
            if trees[i][j] > max_top {
                vert_viz.insert((i, j));
                max_top = trees[i][j];
            }

            let bi = trees.len() - i - 1;
            if trees[bi][j] > max_bottom {
                vert_viz.insert((bi, j));
                max_bottom = trees[bi][j];
            }
        }
    }

    let p1 = hor_viz.union(&vert_viz).count() + always_vis;
    println!("part1: {}", p1);


    let part2 = (1..trees.len() - 1)
        .flat_map(|i| (1..trees[i].len() - 1).map(move |j| (i, j)))
        .map(|(i, j)| calc_sight(&trees, i, j))
        .max();

    println!("part2: {:?}", part2)
}

fn calc_sight(trees: &Vec<Vec<char>>, i: usize, j: usize) -> usize {
    let x = trees[i][j];
    let up = (0..i)
        .rev()
        .take_while(|&r| trees[r][j] < x)
        .count();

    let down = (i + 1..trees.len())
        .take_while(|&r| trees[r][j] < x)
        .count();

    let left = (0..j)
        .rev()
        .take_while(|&c| trees[i][c] < x)
        .count();

    let right = (j + 1..trees.len())
        .take_while(|&c| trees[i][c] < x)
        .count();

    (up + (up != i) as usize)
    * (down + (trees.len() - down - 1 != i) as usize)
    * (left + (left != j) as usize)
    * (right + (trees[i].len() - right - 1 != j) as usize)
}

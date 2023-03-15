use std::cmp;
use std::fs;

fn main() {
    let input = fs::read_to_string("./input").expect("something");

    let coords = input
        .lines()
        .map(|line| {
            line.split(" -> ")
                .map(|tup| {
                    let (x, y) = tup.split_once(",").unwrap();
                    (x.parse::<i32>().unwrap(), y.parse::<i32>().unwrap())
                })
                .collect::<Vec<(i32, i32)>>()
        })
        .collect::<Vec<Vec<(i32, i32)>>>();

    let (minx, maxx, maxy) =
        coords
            .iter()
            .flatten()
            .fold((i32::MAX, 0, 0), |(cminx, cmaxx, cmaxy), (x, y)| {
                (
                    cmp::min(cminx, *x),
                    cmp::max(cmaxx, *x),
                    cmp::max(cmaxy, *y),
                )
            });

    let off = maxy * 3;

    let mut grid = vec![];
    grid.resize_with((maxy + 3) as usize, || {
        vec!['.'; (maxx - minx + off * 2 + 2) as usize]
    });

    for line in coords.iter() {
        for i in 0..line.len() - 1 {
            let (mut sx, mut sy) = line[i];
            let (ex, ey) = line[i + 1];

            let dx = (ex - sx).signum();
            let dy = (ey - sy).signum();

            while sx != ex || sy != ey {
                grid[sy as usize][(sx - minx + off) as usize] = '#';
                sy += dy;
                sx += dx;
            }

            grid[sy as usize][(sx - minx + off) as usize] = '#';
        }
    }

    let startx = 500 - minx + off;
    grid[0][startx as usize] = '+';
    let l = grid[0].len();
    let k = grid.len();
    for x in 0..l {
        grid[k - 1][x] = '#';
    }

    let mut s = 0;
    while let Some((x, y)) = step(&grid, (startx as usize, 0)) {
        if grid[y][x] == '+' {
            break;
        }
        grid[y][x] = 'o';

        //render(&grid);
        //println!("\n");
        s += 1;
    }

    // render(&grid);

    println!("{}", s)
}

fn step(grid: &Vec<Vec<char>>, (mut x, mut y): (usize, usize)) -> Option<(usize, usize)> {
    loop {
        if y >= grid.len() || x == 0 || x >= grid[0].len() {
            return None;
        }

        let nr = &grid[y + 1];
        let new_col = if nr[x] == '.' {
            x
        } else if nr[x - 1] == '.' {
            x - 1
        } else if nr[x + 1] == '.' {
            x + 1
        } else {
            return Some((x, y));
        };

        x = new_col;
        y += 1;
    }
}

#[allow(dead_code)]
fn render(grid: &Vec<Vec<char>>) {
    println!(
        "{}",
        grid.iter()
            .map(|row| row.iter().collect::<String>())
            .collect::<Vec<String>>()
            .join("\n")
    )
}

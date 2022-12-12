use std::fs;
use std::collections::VecDeque;

fn main() {
    let input = fs::read_to_string("./input").expect("omegalol");

    let mut grid = input.lines().map(|line| line.bytes().collect()).collect::<Vec<Vec<u8>>>();

    let start = {
        let mut res: Option<(usize, usize, i32)> = None;
        for y in 0..grid.len() {
            for x in 0..grid[y].len() {
                match grid[y][x] {
                    b'S' => {
                        grid[y][x] = b'a' - 1;
                        res = Some((y, x, 0));
                    },
                    b'E' => {
                        grid[y][x] = b'z' + 1;
                    },
                    _ => {},
                }
            }
        }

        res.unwrap()
    };

    let mut nodes = VecDeque::from([start]);
    let mut result: Option<i32> = None;
    let deltas: &[(i32, i32)] = &[
        (0, 1),
        (0, -1),
        (1, 0),
        (-1, 0)
    ];

    let mut step = 0;
    grid[0][0] = 251;

    while let Some((y, x, dist)) = nodes.pop_front() {
        if grid[y][x] == b'z' + 1 {
            result = Some(dist);
            break;
        }

        if grid[y][x] == 250 {
            continue;
        }

        let lvl = grid[y][x];

        step += 1;
        println!("step {}", step);
        for row in &grid {
            println!("{}", row.iter().map(|&x| if x == 250 { '.' } else { x as char }).collect::<String>());
        }

        for (dy, dx) in deltas {
            let ny = dy + (y as i32);
            let nx = dx + (x as i32);

            //println!("{:?}", (x, y, nx, ny));

            if nx < 0 || ny < 0 {
                continue;
            }

            let sy = ny as usize;
            let sx = nx as usize;

            if sy >= grid.len() || sx >= grid[sy].len() {
                continue;
            }

            println!("inspect {:?}", (grid[sy][sx], lvl, sy, sx));
            if grid[sy][sx] <= lvl + 1 && grid[sy][sx] != 250 {
                println!("{:?}", (grid[sy][sx] as char, lvl as char, grid[sy][sx], lvl));
                nodes.push_back((sy, sx, dist + 1));
            }
        }

        grid[y][x] = 250;
    }

    println!("{:?}", result);


}

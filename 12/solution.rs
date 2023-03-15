use std::collections::VecDeque;
use std::fs;

static VISITED: u8 = 128;
static LVL: u8 = 127;
static DELTAS: &[(i32, i32)] = &[(0, 1), (0, -1), (1, 0), (-1, 0)];

fn main() {
    let input = fs::read_to_string("./input").expect("omegalol");

    let mut grid = input
        .lines()
        .map(|line| line.bytes().collect())
        .collect::<Vec<Vec<u8>>>();

    let start = {
        let mut res: Option<(usize, usize, i32)> = None;
        for y in 0..grid.len() {
            for x in 0..grid[y].len() {
                match grid[y][x] {
                    b'S' => {
                        grid[y][x] = b'a';
                        //res = Some((y, x, 0));
                    }
                    b'E' => {
                        grid[y][x] = b'z';
                        res = Some((y, x, 0));
                    }
                    _ => {}
                }
            }
        }

        res.unwrap()
    };

    let mut nodes = VecDeque::from([start]);

    grid[start.0][start.1] |= VISITED;

    while let Some((y, x, dist)) = nodes.pop_front() {
        let lvl = grid[y][x] & LVL;
        if lvl == b'a' {
            println!("{}", dist);
            break;
        }

        for (dy, dx) in DELTAS {
            let ny = dy + (y as i32);
            let nx = dx + (x as i32);

            if nx < 0 || ny < 0 {
                continue;
            }

            let sy = ny as usize;
            let sx = nx as usize;

            if sy >= grid.len() || sx >= grid[sy].len() {
                continue;
            }

            if grid[sy][sx] & LVL >= lvl - 1 && grid[sy][sx] & VISITED == 0 {
                nodes.push_back((sy, sx, dist + 1));
                grid[sy][sx] |= VISITED;
            }
        }

        //render(&grid);
    }
}

#[allow(dead_code)]
fn render(grid: &Vec<Vec<u8>>) {
    for row in grid {
        println!(
            "{}",
            row.iter()
                .map(|&c| if c & VISITED > 0 { '.' } else { c as char })
                .collect::<String>()
        )
    }

    println!("\n")
}

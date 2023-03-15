use std::collections::HashSet;
use std::fs;

type P2 = (i32, i32);

fn parse_move(s: &str) -> (P2, usize) {
    let deltas = match &s[0..1] {
        "U" => (0, -1),
        "D" => (0, 1),
        "L" => (-1, 0),
        "R" => (1, 0),
        _ => unreachable!(),
    };

    let times = s[2..].parse::<usize>().unwrap();
    (deltas, times)
}

fn main() {
    let input = fs::read_to_string("./input").expect("hurr durr");
    let mut visited = HashSet::from([(0, 0)]);
    let mut tails = vec![(0, 0); 10 /* chage to 2 for part 1 */];

    for (dir, times) in input.lines().map(parse_move) {
        for _ in 0..times {
            tails[0].0 += dir.0;
            tails[0].1 += dir.1;

            for i in 1..tails.len() {
                let (to_x, to_y) = tails[i - 1];
                let (x, y) = tails[i];
                if (to_x - x).abs() > 1 || (to_y - y).abs() > 1 {
                    tails[i] = (x + (to_x - x).signum(), y + (to_y - y).signum());
                }
            }

            visited.insert(*tails.last().unwrap());
        }
    }

    //render(&visited.iter().cloned().collect::<Vec<P2>>());
    println!("{:?}", visited.len());
}

#[allow(dead_code)]
fn render(tails: &Vec<P2>) {
    let mut p = Vec::<Vec<String>>::new();
    let s: i32 = 50;
    p.resize_with(s as usize, || vec![".".to_string(); s as usize]);

    let mut i = 0;
    for &(x, y) in tails.iter() {
        p[(y + s / 2) as usize][(x + s / 2) as usize] = if i == 0 {
            "H".to_string()
        } else {
            char::from_u32(i + 48 as u32).unwrap().to_string()
        };
        i += 1;
    }

    for l in p.iter() {
        println!("{}", l.join(""));
    }
}

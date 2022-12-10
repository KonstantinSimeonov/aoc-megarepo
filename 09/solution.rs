use std::collections::HashSet;
use std::fs;

type P2 = (i32, i32);

fn parse_move(s: &str) -> (P2, usize) {
    let times = s[2..].parse::<usize>().unwrap();
    let deltas = match &s[0..1] {
        "U" => (0, -1),
        "D" => (0, 1),
        "L" => (-1, 0),
        "R" => (1, 0),
        _ => unreachable!(),
    };

    (deltas, times)
}

fn should_move((x1, y1): &P2, (x2, y2): &P2) -> bool {
    (x1 - x2).abs() > 1 || (y1 - y2).abs() > 1
}

fn move_towards((destx, desty): &P2, (tx, ty): &P2) -> P2 {
    let dx = (destx - tx).signum();
    let dy = (desty - ty).signum();

    (dx + tx, dy + ty)
}

fn main() {
    let input = fs::read_to_string("./input").expect("hurr durr");

    let (visited, _) = input.lines().fold(
        (
            HashSet::from([(0, 0)]),
            vec![(0, 0); 10 /* chage to 2 for part 1 */],
        ),
        |(mut visited, mut tails), l| {
            let ((dx, dy), times) = parse_move(l);
            for _ in 0..times {
                tails[0].0 += dx;
                tails[0].1 += dy;

                for i in 1..tails.len() {
                    if should_move(&tails[i - 1], &tails[i]) {
                        tails[i] = move_towards(&tails[i - 1], &tails[i]);
                    }
                }

                visited.insert(*tails.last().unwrap());
            }
            (visited, tails)
        },
    );

    //render(&visited.iter().cloned().collect::<Vec<P2>>());
    println!("{:?}", visited.len());
}

// debug
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

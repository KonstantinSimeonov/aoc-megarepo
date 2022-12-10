use std::collections::HashSet;
use std::fs;

type P2 = (i32, i32);

fn parse_move(s: &str) -> P2 {
    if let [dir, delta] = s.split(' ').collect::<Vec<&str>>()[..] {
        let (sx, sy) = match dir {
            "U" => (0, -1),
            "D" => (0, 1),
            "L" => (-1, 0),
            "R" => (1, 0),
            _ => unreachable!(),
        };

        let d = delta.parse::<i32>().unwrap();
        (d * sx, d * sy)
    } else {
        unreachable!();
    }
}

fn should_move((x1, y1): &P2, (x2, y2): &P2) -> bool {
    (x1 - x2).abs() > 1 || (y1 - y2).abs() > 1
}

fn move_2d((x, y): &P2, (dx, dy): &P2) -> P2 {
    (x + dx, y + dy)
}

fn track_head(set: &mut HashSet<P2>, newh: P2, tail: P2, insert: bool) -> P2 {
    let dx = (newh.0 - tail.0).signum();
    let dy = (newh.1 - tail.1).signum();

    let (mut x, mut y) = tail;

    //println!("{:?}", (oldh, newh, tail, dx, dy));
    //if oldh.0 != newh.0 && oldh.1 != newh.1 {
    //    println!("kewk");
    //}
    //println!("track {:?}", ((x, y), newh, dx, dy));
    while should_move(&(x, y), &newh) {
        //println!("kekekeke{:?}", (x, y));
        // println!("ins {:?}", new_t);
        if x != newh.0 {
            x += dx;
        }

        if y != newh.1 {
            y += dy;
        }

        if insert {
            set.insert((x, y));
        }
    }
    //println!("{:?}", (x, y));

        //new_t = (x - dx, y - dy);
        //set.insert(new_t);
        // println!("ins {:?}", new_t);

    (x, y)
}

fn render(tails: &Vec<P2>) {
    let mut p = Vec::<Vec<String>>::new();
    let s: i32 = 50;
    p.resize_with(s as usize, || vec![".".to_string(); s as usize]);

    let mut i = 0;
    for &(x, y) in tails.iter() {
        //println!("{:?}", (x, y));
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

fn main() {
    let input = fs::read_to_string("./input1").expect("hurr durr");

    let part1 = input.lines().fold(
        (HashSet::from([(0, 0)]), vec![(0, 0); 10]),
        |(mut set, tails), l| {
            let mut nts = tails.clone();
            let m = parse_move(l);
            nts[0] = move_2d(&tails[0], &m);
            println!("{:?}", l);
            for i in 1..tails.len() {
                //println!("{:?}", (last, &tails, i));
                //println!("{:?}", (i, tails.len() - 1));
                nts[i] = track_head(&mut set, nts[i - 1], tails[i], i == tails.len() - 1);
            }
            render(&nts);
            (set, nts)
        },
    );

    //println!("{:?}", part1);
    println!("{:?}", part1.0.len());
}

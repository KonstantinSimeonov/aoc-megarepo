use std::{fs, collections::HashSet};

fn main() {
    let input = fs::read_to_string("./input").expect("noombers m8");

    let mut xs = input
        .trim()
        .lines()
        .map(|l| (l.parse::<i32>().unwrap(), true))
        .collect::<Vec<_>>();

    let mut i: i32 = 0;
    let l = xs.len() as i32;

    while i < l {
        let (delta, can_move) = xs[i as usize];
        if !can_move {
            i += 1;
            continue;
        }

        let d = i + delta;
        let insert_at = if d <= 0 { l - 1 + (d % (l - 1)) } else { d % (l - 1) };

        xs.remove(i as usize);
        xs.insert(insert_at as usize, (delta, false));
    }

    let p = xs.iter().enumerate().find_map(|x| if x.1.0 == 0 { Some(x.0) } else { None }).unwrap();

    let ixs = [p + 1000, p + 2000, p + 3000].map(|ix| xs[(ix % (l as usize))].0);

    println!("{:?}", ixs.iter().sum::<i32>())
}

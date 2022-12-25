use std::fs;

#[derive(Debug)]
struct Num {
    i: usize,
    d: i64,
}

static KEY: i64 = 811589153;

fn main() {
    let input = fs::read_to_string("./input").expect("noombers m8");

    let mut ys = input
        .trim()
        .lines()
        .enumerate()
        .map(|(i, l)| Num {
            i,
            d: l.parse::<i64>().unwrap() * KEY,
        })
        .collect::<Vec<_>>();

    let l1 = ys.len() as i64 - 1;
    let lsize = ys.len();

    let order = ys.iter_mut().map(|r| r as *mut Num).collect::<Vec<_>>();
    let mut xs = order.clone();

    unsafe {
        for _ in 0..10 {
            for &n in order.iter() {
                let Num { i, d } = *n;

                let insert_at = ((l1 + ((i as i64 + d) % l1)) % l1) as usize;
                (*n).i = insert_at;

                xs.remove(i);
                for a in i..lsize - 1 {
                    (*xs[a]).i -= 1;
                }
                xs.insert(insert_at, n);
                for a in (insert_at + 1)..lsize {
                    (*xs[a]).i += 1;
                }
            }
        }

        let p = xs
            .iter()
            .enumerate()
            .find_map(|(i, &x)| if (*x).d == 0 { Some(i) } else { None })
            .unwrap();

        let ixs = [p + 1000, p + 2000, p + 3000].map(|ix| (*xs[ix % lsize]).d);

        println!("{:?}", ixs.iter().sum::<i64>());
    }
}

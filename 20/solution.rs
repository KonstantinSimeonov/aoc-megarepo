use std::fs;

// (mixed position, delta)
type Num = (usize, i64);

fn main() {
    let input = fs::read_to_string("./input").expect("noombers m8");

    let nums = input
        .trim()
        .lines()
        .map(|line| line.parse::<i64>().unwrap())
        .collect::<Vec<_>>();

    let part1 = answer(&mix(&nums, 1, 1));

    println!("part 1: {}", part1);

    let part2 = answer(&mix(&nums, 811589153, 10));

    println!("part 2: {}", part2)
}

fn answer(mixed: &Vec<*mut Num>) -> i64 {
    let p = mixed
        .iter()
        .enumerate()
        .find_map(|(i, &num)| unsafe {
            if (*num).1 == 0 {
                Some(i)
            } else {
                None
            }
        })
        .unwrap();

    let ixs = [p + 1000, p + 2000, p + 3000].map(|ix| unsafe { (*mixed[ix % mixed.len()]).1 });

    ixs.iter().sum()
}

fn mix(numbers: &Vec<i64>, key: i64, times: usize) -> Vec<*mut Num> {
    let mut indexed_pairs = numbers
        .iter()
        .enumerate()
        .map(|(i, &n)| (i, n * key))
        .collect::<Vec<_>>();

    let l1 = indexed_pairs.len() as i64 - 1;
    let len = indexed_pairs.len();

    let order = indexed_pairs
        .iter_mut()
        .map(|r| r as *mut Num)
        .collect::<Vec<_>>();

    let mut mixed = order.clone();

    unsafe {
        for _ in 0..times {
            for &num in order.iter() {
                let (i, delta) = *num;

                let insert_at = ((l1 + ((i as i64 + delta) % l1)) % l1) as usize;
                (*num).0 = insert_at;

                mixed.remove(i);
                for a in i..len - 1 {
                    (*mixed[a]).0 -= 1;
                }
                mixed.insert(insert_at, num);
                for a in (insert_at + 1)..len {
                    (*mixed[a]).0 += 1;
                }
            }
        }
    }

    mixed
}

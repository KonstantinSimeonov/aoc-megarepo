use std::{collections::HashSet, fs, iter, usize};

const FREE: usize = usize::max_value();

fn defrag1(disk: &mut [usize], left: usize, right: usize) {
    if left >= right {
        return;
    }

    if disk[left] != FREE {
        defrag1(disk, left + 1, right);
    } else if disk[right] == FREE {
        defrag1(disk, left, right - 1);
    } else {
        disk[left] = disk[right];
        disk[right] = FREE;
        defrag1(disk, left + 1, right - 1);
    }
}

fn checksum(disk: &[usize]) -> usize {
    disk.iter()
        .zip(0..)
        .filter(|&(bl, _)| *bl != FREE)
        .map(|(id, i)| id * i)
        .sum()
}

fn is_empty(block: &(usize, usize)) -> bool {
    block.1 % 2 == 1
}

fn defrag2(mut disk: Vec<(usize, usize)>) -> Vec<(usize, usize)> {
    let mut right = disk.len() - 1;

    while right > 1 {
        let mut left = 1;
        while left < right {
            if is_empty(&disk[left]) && !is_empty(&disk[right]) && disk[left].0 >= disk[right].0 {
                break;
            }

            left += 1;
        }

        if left >= right {
            right -= 1;
            continue;
        }

        disk[left].0 -= disk[right].0;
        let r = disk[right].clone();
        disk[right] = (r.0, 99);
        disk.insert(left, r);

        right -= 1;
    }

    disk
}

fn reconstruct(disk: &[(usize, usize)]) -> Vec<usize> {
    let mut res = vec![];
    for (len, id) in disk {
        let x = if id % 2 == 0 { id / 2 } else { FREE };
        for _ in 0..*len {
            res.push(x)
        }
    }

    res
}

fn solve(input: &str) -> (usize, usize) {
    let mut blks: Vec<usize> = input
        .trim()
        .bytes()
        .zip(0..)
        .flat_map(|(c, i)| {
            iter::repeat(if i % 2 == 0 { i / 2 } else { FREE }).take((c - b'0') as usize)
        })
        .collect();
    let len = blks.len();
    defrag1(&mut blks, 0, len - 1);
    let a1 = checksum(&blks);

    let blks2: Vec<(usize, usize)> = input
        .trim()
        .bytes()
        .map(|b| (b - b'0') as usize)
        .zip(0..)
        .collect();

    let defragged2 = defrag2(blks2);
    let disk = reconstruct(&defragged2);
    let a2 = checksum(&disk);

    (a1, a2)
}

fn main() {
    let i0 = fs::read_to_string("./09/input0").unwrap();
    println!("{:?}", solve(&i0));

    let i = fs::read_to_string("./09/input").unwrap();
    println!("{:?}", solve(&i));
}

fn p(blocks: &[usize]) {
    for &b in blocks {
        print!(
            "{}",
            if b == FREE {
                ".".to_owned()
            } else {
                b.to_string()
            }
        )
    }

    println!("\nblks")
}

use std::fs;

static PRIMES: &'static [u128] = &[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101];

fn find_start(s: &str, msg_size: usize) -> usize {
    let bytes = s
        .bytes()
        .collect::<Vec<u8>>();

    let start = bytes
        .windows(msg_size)
        .enumerate()
        .find(|(_, codes)| {
            let mut prod: u128 = 1;
            for b in *codes {
                let c = PRIMES[(*b) as usize - 97];
                if prod % c == 0 {
                    return false
                }
                prod *= c;
            }

            return true
        });

    match start {
        Some((i, _)) => i + msg_size,
        _ => 0
    }
}

fn main() {
    let input = fs::read_to_string("./input")
        .expect("stuff");

    let result = input
        .lines()
        .map(|line| (find_start(line, 4), find_start(line, 14)))
        .collect::<Vec<(usize, usize)>>();

    println!("{:?}", result)
}

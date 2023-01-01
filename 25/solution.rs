use std::{collections::VecDeque, fs};

fn to_dec(snafu: &str) -> i64 {
    snafu
        .chars()
        .map(|digit| match digit {
            '0' | '1' | '2' => digit.to_digit(10).unwrap() as i64,
            '-' => -1,
            '=' => -2,
            _ => unreachable!(),
        })
        .fold(0, |num, digit| num * 5 + digit)
}

fn to_snafu(dec: i64) -> String {
    let mut snafu = VecDeque::new();
    let mut left = dec;
    while left > 0 {
        snafu.push_front(left % 5);
        left /= 5;
    }
    snafu.push_front(0);

    // carry
    for i in (1..snafu.len()).rev() {
        let carry = snafu[i] >= 3;
        if carry {
            snafu[i] = (snafu[i] % 3) - 2;
        }

        if snafu[i] < 0 || carry {
            snafu[i - 1] += 1;
        }
    }

    if snafu[0] == 0 {
        snafu.pop_front();
    }

    snafu
        .into_iter()
        .map(|x| match x {
            -2 => '=',
            -1 => '-',
            x => (x as u8 + 48) as char,
        })
        .collect::<String>()
}

fn test() {
    for num in [
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 2022, 12345, 314159265, 1747, 906, 198, 11, 201, 31,
        1257, 32, 353, 107, 37,
    ] {
        let snafu = to_snafu(num);
        let num2 = to_dec(&snafu);
        assert_eq!(num2, num)
    }
}

fn main() {
    test();

    let input = fs::read_to_string("./input").expect("some wonky nums");

    let dec_sum = input.trim().lines().map(to_dec).sum::<i64>();
    let snafu_sum = to_snafu(dec_sum);

    assert_eq!(dec_sum, to_dec(&snafu_sum));
    println!("part 1: {}", snafu_sum);
}

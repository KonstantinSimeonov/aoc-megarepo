use std::fs;

fn byte_to_012(x: u8) -> u32 {
    ((x as u32) - 65) % 23
}

fn main() {
    let input = fs::read_to_string("./input")
        .expect("stuff");

    let result: u32 = input
        .trim()
        .lines()
        .map(|line| (
            line.bytes().next().unwrap(),
            line.bytes().last().unwrap()
        ))
        .map(|(xa, xb)| {
            let a = byte_to_012(xa);
            let b = byte_to_012(xb);
            
            let bonus =
                if a == b { 3  }
                else if (a + 1) % 3 == b { 6 }
                else { 0 };

            b + 1 + bonus
        })
        .sum();

    println!("{:?}", result)
}

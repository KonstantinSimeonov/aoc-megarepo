use std::fs;

fn cycle_value(i: i32, x: i32) -> i32 {
    if i % 40 == 20 {
        i * x
    } else {
        0
    }
}

fn pixel_value(i: i32, x: i32) -> bool {
    ((i % 40) - x).abs() <= 1
}

fn main() {
    let input = fs::read_to_string("./input").expect("hkjshg");

    let mut x = 1;
    let mut cycle = 0;

    let mut part1 = 0;
    let mut screen = vec![false; 241];

    for l in input.lines() {
        match l {
            "noop" => {
                // draw
                screen[cycle as usize] = pixel_value(cycle, x);

                // advance cycles
                cycle += 1;

                part1 += cycle_value(cycle, x);
            }
            add_cmd => {
                // draw
                screen[cycle as usize] = pixel_value(cycle, x);

                // advance
                cycle += 1;

                part1 += cycle_value(cycle, x);

                // draw after advancing one cycle
                screen[cycle as usize] = pixel_value(cycle, x);

                // advance
                cycle += 1;

                part1 += cycle_value(cycle, x);

                // 2 cycles have passes, write into x
                x += add_cmd[5..].parse::<i32>().unwrap();
            }
        }
    }

    println!("{:?}", part1);

    let part2 = screen
        .chunks(40)
        .map(|line| {
            line.iter()
                .map(|&b| if b { "#" } else { " " })
                .collect::<Vec<&str>>()
                .join("")
        })
        .collect::<Vec<String>>()
        .join("\n");

    println!("{}", part2);
}

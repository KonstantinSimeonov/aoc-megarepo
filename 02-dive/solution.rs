use std::fs;

fn main() {
    let input = fs::read_to_string("./input")
        .expect("To read the file");
    let (x, y1, aim, y2) = input.lines()
        .map(|x| {
            let (cmd, d) = if let [cmd, delta] = x.split(" ").collect::<Vec<_>>()[..] {
                (cmd, delta.parse::<i32>().unwrap())
            } else {
                ("", 0)
            };
            match cmd {
                "forward" => (d, 0),
                "down" => (0, d),
                "up" => (0, -d),
                _ => (0, 0)
            }
        })
        .fold(
            (0, 0, 0, 0),
            |(x, y1, aim, y2), (dx, dy)| (x + dx, y1 + dy, aim + dy, y2 + dx * aim)
        );

    println!("part 1: {}, aim: {}, part 2: {}", x * y1, aim, x * y2)
}

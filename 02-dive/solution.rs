use std::fs;

fn main() {
    let input = fs::read_to_string("./input")
        .expect("To read the file");
    let (x, y, aim, y2) = input.lines()
        .map(|x| match x.split(" ").collect::<Vec<_>>()[..] {
            ["forward", d] => (d.parse::<i32>().unwrap(), 0),
            ["down", d] => (0, d.parse::<i32>().unwrap()),
            ["up", d] => (0, -d.parse::<i32>().unwrap()),
            _ => (0, 0)
        })
        .fold(
            (0, 0, 0, 0),
            |(x, y1, aim, y2), (dx, dy)| (x + dx, y1 + dy, aim + dy, y2 + dx * aim)
        );

    println!("part 1: {}, aim: {}, part 2: {}", x * y, aim, x * y2)
}

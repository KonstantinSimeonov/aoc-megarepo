use std::fs;

fn main() {
    let input = fs::read_to_string("./input").expect("stuff");

    let (mut stacks, moves) = {
        let mut split_it = input.split("\n\n");

        let init = {
            let mut stacks: Vec<Vec<char>> = split_it
                .next()
                .unwrap()
                .split("\n")
                .map(|line| line.chars().skip(1).step_by(4).collect())
                .collect();

            stacks.pop();

            // transpose for ez clap
            (0..stacks[0].len())
                .map(
                    // .rev() because stack lol
                    |i| {
                        stacks
                            .iter()
                            .rev()
                            .map(|v| v[i])
                            .filter(|&c| c != ' ')
                            .collect()
                    },
                )
                .collect::<Vec<Vec<char>>>()
        };

        let instructions = split_it
            .next()
            .unwrap()
            .lines()
            .map(|line| match line.split(' ').collect::<Vec<&str>>()[..] {
                [_, count, _, from, _, to] => (
                    count.parse::<usize>().unwrap(),
                    from.parse::<usize>().unwrap() - 1,
                    to.parse::<usize>().unwrap() - 1,
                ),
                _ => unreachable!(),
            })
            .collect::<Vec<(usize, usize, usize)>>();

        (init, instructions)
    };

    for (count, from, to) in moves {
        let r = stacks[from].len() - count..;
        let crates = stacks[from]
            .drain(r)
            .rev() // remove the .rev() call for part2
            .collect::<Vec<char>>();

        for c in crates.into_iter() {
            stacks[to].push(c);
        }
    }

    let result = stacks
        .into_iter()
        .map(|v| v.last().unwrap().clone())
        .collect::<String>();
    println!("{}", result);
}

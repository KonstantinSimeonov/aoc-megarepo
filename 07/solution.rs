use std::fs;
use std::collections::HashMap;

type DirMap<'a> = HashMap<String, Vec<(&'a str, u32, bool)>>;

// display the dirs for lolz
fn tree(dirs: &DirMap, start: &str, depth: usize) {
    match dirs.get(start) {
        Some(files) => {
            let padding = " ".repeat(depth * 2);
            println!("{}- {} (dir)", padding, start);
            for (name, size, is_dir) in files.iter() {
                if *is_dir {
                    let next_name = start.to_owned() + "/" + name;
                    tree(dirs, &next_name, depth + 1);
                } else {
                    println!("  {}- {} (file, size={})", padding, name, size);
                }
            }
        }
        None => println!("no such dir {}", start)
    }
}

fn calc_dir_sizes(dirs: &DirMap, start: &str, sizes: &mut HashMap<String, u32>) {
    match dirs.get(start) {
        Some(files) => {
            let mut sum = 0;
            for (name, size, is_dir) in files.iter() {
                if *is_dir {
                    let next_name = start.to_owned() + "/" + name;
                    calc_dir_sizes(dirs, &next_name, sizes);
                    sum += sizes.get(&next_name as &str).unwrap();
                } else {
                    sum += size;
                }
            }

            sizes.insert(start.to_string(), sum);
        },
        _ => println!("kekw")
    }
}

fn main() {
    let input = fs::read_to_string("./input")
        .expect("stuff");

    let (map, _): (DirMap, Vec<&str>) = input
        .lines()
        .fold(
            (HashMap::new(), vec![]),
            |(mut dirs, mut cwd_stack), b| match b.split(' ').collect::<Vec<&str>>()[..] {
                ["$", "cd", ".."] => {
                    cwd_stack.pop();
                    (dirs, cwd_stack)
                },
                ["$", "cd", dir_name] => {
                    cwd_stack.push(dir_name);
                    dirs.insert(cwd_stack.join("/"), vec![]);
                    (dirs, cwd_stack)
                },
                ["$", "ls"] => (dirs, cwd_stack),
                [a, b] => {
                    let dir_name = cwd_stack.join("/");
                    let children = dirs
                        .get_mut(&dir_name).unwrap();

                    children.push(
                        (b, a.parse::<u32>().unwrap_or(0), a == "dir")
                    );
                    (dirs, cwd_stack)
                },
                _ => (dirs, cwd_stack)
            }
        );

    //println!("{:?}\n", map);

    //tree(&map, "/", 0);

    let mut sizes_map = HashMap::new();
    calc_dir_sizes(&map, "/", &mut sizes_map);
    //println!("\n{:?}", sizes);

    let used_space = *sizes_map.get("/").unwrap();
    let sizes = sizes_map.into_values().collect::<Vec<u32>>();
    let part1: u32 = sizes.iter().filter(|v| **v <= 100000).sum();

    // part 2
    let part2 = sizes.iter().filter(|s| used_space - **s < 40000000).min();
    println!("part 1: {}, part 2: {:?}", part1, part2)
}

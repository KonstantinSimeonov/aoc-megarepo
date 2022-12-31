use std::{fs, collections::{HashSet, HashMap, VecDeque}};

type P = (i32, i32);
type Coords = HashSet<P>;

const DIRS: [[P; 3]; 4] = [
    [(-1, -1), (-1, 0), (-1, 1)],
    [(1, -1), (1, 0), (1, 1)],
    [(-1, -1), (0, -1), (1, -1)],
    [(-1, 1), (0, 1), (1, 1)]
];

fn propose_move(elves: &Coords, dirs: &VecDeque<[P; 3]>) -> Coords {
    let xs = elves
        .iter()
        .map(|&(y, x)| {
            let d = dirs
                .iter()
                .filter(|&ds| ds.iter().all(|&(dy, dx)| !elves.contains(&(y + dy, x + dx))))
                .collect::<Vec<_>>();

            if d.len() == 4 {
                return ((y, x), None)
            }

            ((y, x), d.first().map(|ds| match ds[1] {
                (dy, dx) => (y + dy, x + dx)
            }))
        })
        .fold(
            HashMap::<Option<P>, Vec<P>>::new(),
            |mut props, (pos, prop)| {
                props.entry(prop).and_modify(|sug| sug.push(pos.clone())).or_insert(vec![pos]);
                props
            }
        );
    //render(&elves);

    //println!("{:?}", &xs);

    let after_move = xs
        .into_iter()
        .flat_map(|(dest, goers)| match dest {
            Some(pos) if goers.len() == 1 => vec![pos],
            _ => goers
        })
        .collect::<Coords>();

    //render(&after_move);

    //println!("{:?}", after_move)
    after_move
}

fn main() {
    let input = fs::read_to_string("./input").expect("elves");

    let coords = input
        .lines()
        .enumerate()
        .flat_map(|(i, line)| {
            line.chars()
                .enumerate()
                .filter_map(move |(j, c)| if c == '#' { Some((i as i32, j as i32)) } else { None })
        })
        .collect::<Coords>();

    let mut dirs = VecDeque::from(DIRS);

    let mut cs = coords;
    let mut r = 1;
    loop {
        //println!("round {}", r);
        let cs1 = propose_move(&cs, &dirs);
        //render(&cs1);

        if cs1 == cs {
            println!("finished on {}", r);
            return
        }

        cs = cs1;

        let d = dirs.pop_front().unwrap();
        dirs.push_back(d);

        r += 1;
    }

    //println!("{:?}", coords)

    let part1 = count_empty(&cs);

    println!("{}", part1)
}

fn rect(coords: &Coords) -> (i32, i32, i32, i32) {
    let (mut ys, mut xs): (Vec<_>, Vec<_>) = coords.clone().into_iter().unzip();

    ys.sort();
    xs.sort();

    (
        ys[0],
        ys[ys.len() - 1],
        xs[0],
        xs[xs.len() - 1],
    )
}

fn count_empty(coords: &Coords) -> i32 {
    let (mut sy, ey, sx, ex) = rect(coords);

    let mut empty = 0;

    while sy <= ey {
        let mut x = sx;
        while x <= ex {
            empty += if coords.contains(&(sy, x)) { 0 } else { 1 };
            x += 1;
        }
        sy += 1;
    }

    empty
}

fn render(coords: &Coords) {
    let (mut sy, ey, sx, ex) = rect(coords);

    while sy <= ey {
        let mut x = sx;
        while x <= ex {
            print!("{}", if coords.contains(&(sy, x)) { "#" } else { "." });
            x += 1;
        }

        println!();
        sy += 1;
    }

    println!()
}

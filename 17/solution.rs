use std::fs;
use std::iter;
use std::iter::Peekable;

static WALLS: i32 = (1 << 8) + 1;
static FLOOR: i32 = (1 << 9) - 1;

fn repeat_vec<T: Copy>(xs: &Vec<T>, name: String) -> impl Iterator<Item = T> + '_ {
    let mut i = 0;
    let it = iter::repeat_with(move || {
        let x = xs[i];
        //println!("{}: {} {}", name, i, xs.len());
        i = (i + 1) % xs.len();
        x
    });

    it
}

fn main() {
    let input = fs::read_to_string("./input")
        .expect("lol")
        .trim()
        .chars()
        .map(|dir| dir == '<')
        .collect::<Vec<_>>();

    let mut dirs = repeat_vec(&input, "dirs".to_string());

    let mut stack = vec![FLOOR];
    let ps = Vec::from([
        [60, 0, 0, 0],
        [16, 56, 16, 0],
        [56, 8, 8, 0],
        [32, 32, 32, 32],
        [48, 48, 0, 0],
    ]);

    let mut rocks = repeat_vec(&ps, "rocks".to_string());

    let mut s = 0;

    let upper: usize = 1_000_000_000_000;
    for i in 0..upper {
        if i % 100000 == 0 {
            println!("{} left", upper - i);
        }
        let (wi, _) = stack
            .iter()
            .enumerate()
            .rev()
            .find(|&(_, x)| *x != WALLS)
            .unwrap();
        let diff = stack.len() - wi;
        stack.extend((0..5 - diff).map(|_| WALLS));
        //println!("pad");
        //render(&stack);

        //println!("fall {}", i);
        if let Some(cut_ind) = fall(&rocks.next().unwrap(), &mut dirs, &mut stack) {
            s += cut_ind;
            stack.splice(0..cut_ind, []);
        }
        //render(&stack);
    }

    let pos = stack.iter().rev().position(|&x| x != WALLS).unwrap();

    println!("{:?}", s + stack.len() - pos - 1);

    //let stuff = stack.iter().enumerate().filter(|&(i, x)| *x == FLOOR).collect::<Vec<_>>();
    //println!("{:?}", stuff)
}

fn fall(rock: &[i32; 4], dirs: &mut impl Iterator<Item = bool>, stack: &mut Vec<i32>) -> Option<usize> {
    //let mut dbg = Vec::new();
    let mut i = stack.len() - 1;
    let mut r = *rock;
    loop {
        let d = dirs.next().unwrap();
        //dbg.push(d);
        let moved_r = push(&r, d);
        if !collides(&moved_r, i, stack) {
            //println!("move {} {}", i, if d { "left" } else { "right" });
            r = moved_r;
        } else {
            //println!("cannot move {}", if d { "left" } else { "right" });
        }

        i = i - 1;

        if collides(&r, i, stack) {
            break;
        }
    }

    //println!("collision {}", i);
    //println!("{}", dbg.into_iter().map(|x| if x { '<' } else { '>' }).collect::<String>());
    let mut cut = None;
    for j in 0..r.len() {
        let ind = i + j + 1;
        stack[ind] |= r[j];
        if stack[ind] == FLOOR {
            cut = Some(ind);
        }
    }

    cut
}

fn render(stack: &Vec<i32>) {
    stack
        .iter()
        .enumerate()
        .rev()
        .map(|(i, row)| {
            if i == 0 {
                "+-------+".to_string()
            } else {
                format!("{:09b}", row)
                    .chars()
                    .enumerate()
                    .map(|(j, c)| {
                        if j == 0 || j == 8 {
                            '|'
                        } else if c == '1' {
                            '#'
                        } else {
                            '.'
                        }
                    })
                    .collect::<String>()
            }
        })
        .for_each(|x| println!("{}", x));
    println!("\n");
}

fn collides(rock: &[i32; 4], i: usize, stack: &Vec<i32>) -> bool {
    let res = rock
        .iter()
        .enumerate()
        .filter(|&(i1, row)| (i + i1) < stack.len() && stack[i + i1] & row != 0)
        .collect::<Vec<_>>();

    res.len() > 0
}

fn left(x: i32) -> i32 {
    x << 1
}

fn right(x: i32) -> i32 {
    x >> 1
}

fn push(&[a, b, c, d]: &[i32; 4], l: bool) -> [i32; 4] {
    let f = if l { left } else { right };
    [f(a), f(b), f(c), f(d)]
}

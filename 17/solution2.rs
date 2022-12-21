//use std::collections::HashMap;
use std::fs;
use std::iter;

static WALLS: i32 = (1 << 8) + 1;
static FLOOR: i32 = (1 << 9) - 1;

fn repeat_vec<T: Copy>(xs: &Vec<T>, start: usize) -> impl Iterator<Item = (usize, T)> + '_ {
    let mut i = start;
    let it = iter::repeat_with(move || {
        let x = xs[i];
        //println!("{}: {} {}", name, i, xs.len());
        i = (i + 1) % xs.len();
        (i, x)
    });

    it
}

fn calc_streaks<T: Eq>(xs: &Vec<T>, start: usize, end: usize) -> Option<(usize, usize, usize)> {
    let x = &xs;
    for i in start..end {
        let mut j = i + 1;

        while j < end {
            if x[i] == x[j] {
                let mut j1 = j;

                let mut i1 = i;
                assert!(i1 != j1);
                assert!(j1 >= end || x[i1] == x[j1]);

                while j1 < end && x[i1] == x[j1] {
                    i1 += 1;
                    j1 += 1;
                }

                let s = j1 - i1;
                if j1 == end && i1 > j {
                    return Some((i, j, (j1 - i) / s));
                }
            }

            j += 1;
        }
    }

    None
}

fn fall_rocks(gasses: &Vec<bool>, rocks_count: usize) -> (Vec<i32>, Vec<(usize, usize, usize)>) {
    let mut dirs = repeat_vec(&gasses, 0);
    let mut stack = vec![FLOOR];
    let ps = Vec::from([
        [60, 0, 0, 0],
        [16, 56, 16, 0],
        [56, 8, 8, 0],
        [32, 32, 32, 32],
        [48, 48, 0, 0],
    ]);

    let mut rocks = repeat_vec(&ps, 0);

    let mut hs = vec![];

    // 24_596 -> 26,79,702
    for _ in 0..rocks_count {
        let (wi, _) = stack
            .iter()
            .enumerate()
            .rev()
            .find(|&(_, x)| *x != WALLS)
            .unwrap();
        let diff = stack.len() - wi;
        stack.extend((0..5 - diff).map(|_| WALLS));
        let (ri, cr) = rocks.next().unwrap();
        let di = fall(&cr, &mut dirs, &mut stack);

        while *stack.last().unwrap() == WALLS {
            stack.pop();
        }
        hs.push((stack.len() - 1, di, ri));
    }

    (stack, hs)
}

// (0..1514285714288)
// 1504093567270
// 1504093567271
// 1504093567242
// 1504093567239
fn main() {
    let input = fs::read_to_string("./input0")
        .expect("lol")
        .trim()
        .chars()
        .map(|dir| dir == '<')
        .collect::<Vec<_>>();

    let (stack, hs) = fall_rocks(&input, 20_000);

    println!("part 1: {}", stack.len() - 1);

    let deltas = hs.iter().zip(hs.iter().skip(1)).map(|(prev, curr)| (curr.0 - prev.0, curr.1, curr.2)).collect::<Vec<_>>();

    //println!("part 1: {:?}", deltas);//stack.len() - pos - 1);
    let str = calc_streaks(&deltas, 0, deltas.len()).expect("to have repeating pattern");
    println!("repeating pattern {:?}", str);
    let (s, e, _) = str;
    let pat = &deltas[s..e];
    let pat_sum: usize = pat.iter().map(|t3| t3.0).sum();
    let pat_len = e - s;
    println!("{} {}", pat_sum, pat_len);

    let init_sum: usize = deltas.iter().take(s).map(|t3| t3.0).sum::<usize>() - 1;
    println!("gosho {} pesho", init_sum);

    let big: usize = 1000000000000;

    let without_start = (big - s) / pat_len;
    let rem = (big - s) % pat_len;

    println!("{:?}", (init_sum, without_start, rem));

    let rep_sum = without_start * pat_sum;

    let ans = rep_sum + init_sum;

    println!("{}", 1514285714288 - ans)
    //println!("{} {}", stack[stack.len() - 1] == WALLS, stack[stack.len() - 2] == WALLS);

    //let strs = calc_streaks(&stack, 0, stack.len());
    //println!("{:?}", strs);
    //println!("{:?}", stack.iter().rev().take((79 - 26) + 1).rev().collect::<Vec<_>>().as_slice());
    //println!("{:?}", &stack[26..79]);
    //println!("window.data = {:?};", stack.as_slice());
}

fn fall(
    rock: &[i32; 4],
    dirs: &mut impl Iterator<Item = (usize, bool)>,
    stack: &mut Vec<i32>,
) -> usize {
    //let mut dbg = Vec::new();
    let mut i = stack.len() - 1;
    let mut r = *rock;
    let mut di = 0;
    loop {
        let (xi, d) = dirs.next().unwrap();
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
            di = xi;
            break;
        }
    }

    //println!("collision {}", i);
    //println!("{}", dbg.into_iter().map(|x| if x { '<' } else { '>' }).collect::<String>());
    for j in 0..r.len() {
        let ind = i + j + 1;
        stack[ind] |= r[j];
    }

    di
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

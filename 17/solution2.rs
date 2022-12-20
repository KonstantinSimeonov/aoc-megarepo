use std::fs;
use std::iter;

static WALLS: i32 = (1 << 8) + 1;
static FLOOR: i32 = (1 << 9) - 1;

fn repeat_vec<T: Copy>(xs: &Vec<T>) -> impl Iterator<Item = (usize, T)> + '_ {
    let mut i = 0;
    let it = iter::repeat_with(move || {
        let x = xs[i];
        //println!("{}: {} {}", name, i, xs.len());
        i = (i + 1) % xs.len();
        (i, x)
    });

    it
}

// (0..1514285714288)
// 1504093567270
fn main() {
    let input = fs::read_to_string("./input")
        .expect("lol")
        .trim()
        .chars()
        .map(|dir| dir == '<')
        .collect::<Vec<_>>();

    let mut dirs = repeat_vec(&input);

    let mut stack = vec![FLOOR];
    let ps = Vec::from([
        [60, 0, 0, 0],
        [16, 56, 16, 0],
        [56, 8, 8, 0],
        [32, 32, 32, 32],
        [48, 48, 0, 0],
    ]);

    let mut rocks = repeat_vec(&ps);

    let mut s = 0;

    let mut res = vec![];

    let upper: usize = 1000000;
    let mut li = 0;
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
        let (_, cr) = rocks.next().unwrap();
        if let Some((cut_ind, di)) = fall(&cr, &mut dirs, &mut stack) {
            s += cut_ind;
            res.push((stack.splice(0..cut_ind, []).collect::<Vec<_>>(), i - li, di));
            li = i;
        }
        //render(&stack);
    }

    let pos = stack.iter().rev().position(|&x| x != WALLS).unwrap();

    println!("part 1: {:?}", s + stack.len() - pos - 1);
    //println!("{:?}", res);

    let (bsum, brocks) = res.iter().take(5).fold((0, 0), |(s1, s2), (x, y, _)| (s1 + x.len(), s2 + y));
    println!("start {:?}", (bsum, brocks));

    let mut scs = vec![];
    for i in 5..28 {
        for j in (i + 1)..res.len() {
            if res[i].0 == res[j].0 {
                //println!("match {:?}", (i, j, res[i].0.len(), res[i].1, res[j].1, res[i].2, res[j].2));
                scs.push((res[i].0.len(), res[i].1, res[i].2));
                break;
            }
        }
    }

    println!("{:?}", scs);

    let (sheight, srocks): (usize, usize) = scs.iter().fold((0, 0), |(sum_height, sum_rocks), (h, r, _)| (sum_height + h, sum_rocks + r));
    let u: usize = 1000000000000;
    println!("{:?}", (sheight, srocks));

    let fits = (u - brocks) / srocks;
    let rem = (u - brocks) % srocks;
    println!("rem {:?}", rem);

    let ans = fits * sheight;
    println!("psums {:?}", scs.iter().map(|x| x.1).scan(0, |s, x| { *s += x; Some(*s) }).collect::<Vec<_>>());
    let rem_sum = 1282;

    let x = bsum + ans + rem_sum;
    assert!(x < 1514285714288);
    println!("{:?}\n1514285714288\n{:?}", x, (bsum, ans, rem_sum));

    //let fsum: usize = res.iter().take(5).map(|x| x.0.len()).sum();
    //let msum = fits * s;
    //let lsum: usize = scs.iter().take(rem).sum();

    //println!("{:?} {} {}", &scs, scs.len(), s);
    //println!("{} {} {}\n55913043478085\n1514285714288", fsum, msum, lsum);

    //let res2 = res.iter().skip(5).map(|x| x.0.len()).collect::<Vec<_>>();
    //println!("{:?}", res2.chunks(23).map(|c| c.iter().sum()).collect::<Vec<usize>>())
    //let stuff = stack.iter().enumerate().filter(|&(i, x)| *x == FLOOR).collect::<Vec<_>>();
    //println!("{:?}", stuff)
}

fn fall(rock: &[i32; 4], dirs: &mut impl Iterator<Item = (usize, bool)>, stack: &mut Vec<i32>) -> Option<(usize, usize)> {
    //let mut dbg = Vec::new();
    let mut i = stack.len() - 1;
    let mut r = *rock;
    let mut di: usize = 0;
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
    let mut cut = None;
    for j in 0..r.len() {
        let ind = i + j + 1;
        stack[ind] |= r[j];
        if stack[ind] == FLOOR {
            cut = Some((ind, di));
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

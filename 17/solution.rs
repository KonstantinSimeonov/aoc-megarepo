use std::fs;
use std::iter;

fn main() {
    let input = fs::read_to_string("./input")
        .expect("lol");

    let dirs = input
        .trim()
        .chars()
        .map(|dir| dir == '<')
        .collect::<Vec<_>>();

    // 15_000 seems big enough for the pattern to start repeating
    let heights = fall_rocks(&dirs, 15_000);
    println!("part 1: {}", heights[2021]);

    let part2 = fall_rocks_big(&heights, 1000000000000);
    println!("part 2: {}", part2);
    //println!("window.data = {:?}", heights);
}

fn repeat_vec<T>(xs: &Vec<T>) -> impl Iterator<Item = &T> + '_ {
    let mut i = 0;
    let it = iter::repeat_with(move || {
        let x = &xs[i];
        i = (i + 1) % xs.len();
        x
    });

    it
}

fn find_repeating_pattern<T: Eq>(xs: &Vec<T>) -> Option<(usize, usize, usize)> {
    for i in 0..xs.len() {
        let mut j = i + 1;

        while j < xs.len() {
            if xs[i] == xs[j] {
                let mut j1 = j;
                let mut i1 = i;

                while j1 < xs.len() && xs[i1] == xs[j1] {
                    i1 += 1;
                    j1 += 1;
                }

                if j1 == xs.len() && i1 > j {
                    return Some((i, j, (j1 - i) / (j1 - i1)));
                }
            }

            j += 1;
        }
    }

    None
}

static WALLS: i32 = (1 << 8) + 1;
static FLOOR: i32 = (1 << 9) - 1;

fn fall_rocks(gasses: &Vec<bool>, rocks_count: usize) -> Vec<usize> {
    let shapes = Vec::from([
        [60, 0, 0, 0],
        [16, 56, 16, 0],
        [56, 8, 8, 0],
        [32, 32, 32, 32],
        [48, 48, 0, 0],
    ]);

    let mut dirs = repeat_vec(&gasses);
    let mut rocks = repeat_vec(&shapes);

    let mut heights = vec![];
    let mut stack = vec![FLOOR];

    for _ in 0..rocks_count {
        stack.extend((0..4).map(|_| WALLS));
        let rock = rocks.next().unwrap();
        fall(&rock, &mut dirs, &mut stack);

        while *stack.last().unwrap() == WALLS {
            stack.pop();
        }
        heights.push(stack.len() - 1);
    }

    heights
}

fn fall_rocks_big(heights: &Vec<usize>, target: usize) -> usize {
    let deltas = heights.iter()
        .zip(heights.iter().skip(1))
        .map(|(prev, curr)| curr - prev)
        .collect::<Vec<_>>();

    let pattern_range = find_repeating_pattern(&deltas).expect("to have repeating pattern");
    println!("repeating pattern {:?}", pattern_range);
    let (start, end, _) = pattern_range;
    let pattern = &deltas[start..end];
    let pattern_height: usize = pattern.iter().sum();

    let non_pattern_height: usize = deltas.iter().take(start).sum();

    let repeating = target - start;

    let full_repeats = repeating / pattern.len();
    let full_repeats_height = full_repeats * pattern_height;

    let last_pattern = repeating % pattern.len();
    let last_pattern_height: usize = pattern.iter().take(last_pattern).sum();

    let target_height = full_repeats_height + non_pattern_height + last_pattern_height;

    target_height
}

fn fall<'a>(
    rock: &[i32; 4],
    dirs: &mut impl Iterator<Item = &'a bool>,
    stack: &mut Vec<i32>,
) {
    let mut i = stack.len() - 1;
    let mut r = *rock;
    while !collides(&r, i, stack) {
        let moved_r = push_side(&r, *dirs.next().unwrap());
        if !collides(&moved_r, i, stack) {
            r = moved_r;
        }

        i = i - 1;
    }

    for j in 0..r.len() {
        let ind = i + j + 1;
        stack[ind] |= r[j];
    }
}

fn collides(rock: &[i32; 4], i: usize, stack: &Vec<i32>) -> bool {
    let res = rock
        .iter()
        .enumerate()
        .filter(|&(i1, row)| (i + i1) < stack.len() && stack[i + i1] & row != 0)
        .collect::<Vec<_>>();

    res.len() > 0
}

fn push_side(&[a, b, c, d]: &[i32; 4], l: bool) -> [i32; 4] {
    let f = if l { left } else { right };
    [f(a), f(b), f(c), f(d)]
}

fn left(x: i32) -> i32 {
    x << 1
}

fn right(x: i32) -> i32 {
    x >> 1
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

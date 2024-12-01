use std::fs;
use std::iter;
use std::collections::HashMap;

fn solve(input: String) -> (i32, i32) {
  let (mut fst, mut snd): (Vec<_>, Vec<_>) = input
    .trim()
    .split("\n")
    .map(|line| {
      let (l, r) = line.split_once("   ").unwrap();
      (l.parse::<i32>().unwrap(), r.parse::<i32>().unwrap())
    }).unzip();

  fst.sort();
  snd.sort();

  let freq = snd.iter().fold(
    HashMap::<i32, i32>::new(),
    |mut f, &x| {
      *f.entry(x).or_default() += 1;
      f
    }
  );

  let a2: i32 = fst.iter().map(|x| freq.get(x).unwrap_or(&0) * x).sum();

  let a1 = iter::zip(fst, snd).map(|(a, b)| (a - b).abs()).sum();

  (a1, a2)
}

fn main() {
  let input0 = fs::read_to_string("./01/input0").expect("stuff");
  let input = fs::read_to_string("./01/input").expect("stuff");

  println!("test {:?}", solve(input0));
  println!("answer {:?}", solve(input));
}

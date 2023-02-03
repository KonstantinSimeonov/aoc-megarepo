use std::fs;
use std::collections::HashSet;
use std::iter::FromIterator;
//use std::io::BufReader;
//use std::io::BufRead;

fn prio(c: char) -> u32 {
    ((c as u32) - 64) % 32 + (26 * (1 - ((c as u32) / 97)))
}

fn part1(s: &str) -> u32 {
    return s.lines()
        .map(|line| {
            let l = line.len();
            let common = line[0..l / 2]
                .chars()
                .filter(|c| line[l / 2..l].contains(*c))
                .collect::<HashSet<char>>();
            common.into_iter().fold(0, |a, b| a + prio(b))
        })
        .fold(0, |a, b| a + b);
        //.collect::<Vec<u32>>();
}

fn part2(s: &str) -> u32 {
    let xs: u32 = s.lines()
        .collect::<Vec<&str>>()
        .as_slice()
        .chunks(3)
        .map(
            |xs| xs
                .into_iter()
                .map(|x| x.chars())
                .map(HashSet::from_iter)
                .reduce(|a: HashSet<char>, b| &a & &b)
                .unwrap()
                .iter()
                .map(|x| prio(*x))
                .next()
                .unwrap()
        )
        .sum();
    xs
}

//struct ChonksIter<'a, B: BufRead> {
//    lines: &'a std::io::Lines<B>,
//    n: usize
//}
//
//impl<'a, B: BufRead> Iterator for ChonksIter<'a, B> {
//    type Item = Vec<String>;
//
//    fn next(&mut self) -> Option<Self::Item> {
//        let v = self.lines.take(self.n).collect::<Result<Vec<String>, _>>();
//        match v {
//            Ok(xs) => Some(xs),
//            _ => None
//        }
//    }
//}
//
//trait Chonkable<B: BufRead> {
//    fn chonks(&self, n: usize) -> ChonksIter<B>;
//}
//
//impl<B: BufRead> Chonkable<B> for std::io::Lines<B> {
//    fn chonks(&self, n: usize) -> ChonksIter<B> {
//        ChonksIter {
//            lines: self,
//            n: n
//        }
//    }
//}

fn main() {
    //let f = File::open("./input0").expect("stuff");
    //let mut r = BufReader::new(f);

    //for x in r.lines().chunks(3) {
    //    println("{:?}", x);
    //}
    let input = fs::read_to_string("./input")
        .expect("stuff");

    println!("{:?}", part1(&input));
    println!("{:?}", part2(&input));

    //let v1: Vec<&[i32]> = [1, 2, 3, 4, 5, 6].chunks(3).collect();
    //println!("{:?}", v1)
}

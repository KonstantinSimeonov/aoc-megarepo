#[derive(Clone, Debug)]
struct Monkey {
    items: Vec<usize>,
    op: fn(usize) -> usize,
    test: usize,
    targets: Vec<usize>,
    score: usize,
}

fn simulate_rounds(monkeys: &mut Vec<Monkey>, rounds: usize, calc_worry: &Fn(usize) -> usize) -> usize {
    for _ in 0..rounds {
        for i in 0..monkeys.len() {
            monkeys[i].score += monkeys[i].items.len();
            for _ in 0..monkeys[i].items.len() {
                let item = monkeys[i].items.remove(0);
                let worry = calc_worry((monkeys[i].op)(item));
                let target = monkeys[i].targets[(worry % monkeys[i].test != 0) as usize];
                monkeys[target].items.push(worry);
            }
        }
    }

    monkeys.sort_by(|a, b| b.score.cmp(&a.score));

    if let [m1, m2] = &monkeys[0..2] {
        m1.score * m2.score
    } else {
        unreachable!();
    }
}

fn main() {
    let mut monkeys0 = vec![
        Monkey { items: vec![79, 98], op: |old| old * 19, test: 23, targets: vec![2, 3], score: 0 },
        Monkey { items: vec![54, 65, 75, 74], op: |old| (old + 6), test: 19, targets: vec![2, 0], score: 0 },
        Monkey { items: vec![79, 60, 97], op: |old| old * old, test: 13, targets: vec![1, 3], score: 0 },
        Monkey { items: vec![74], op: |old| (old + 3), test: 17, targets: vec![0, 1], score: 0 },
    ];

    let mut monkeys1 = vec![
        Monkey { items: vec![93, 98], op: |old| old * 17, test: 19, targets: vec![5, 3], score: 0 },
        Monkey { items: vec![95, 72, 98, 82, 86], op: |old| old + 5, test: 13, targets: vec![7, 6], score: 0 },
        Monkey { items: vec![85, 62, 82, 86, 70, 65, 83, 76], op: |old| old + 8, test: 5, targets: vec![3, 0], score: 0 },
        Monkey { items: vec![86, 70, 71, 56], op: |old| old + 1, test: 7, targets: vec![4, 5], score: 0 },
        Monkey { items: vec![77, 71, 86, 52, 81, 67], op: |old| old + 4, test: 17, targets: vec![1, 6], score: 0 },
        Monkey { items: vec![89, 87, 60, 78, 54, 77, 98], op: |old| old * 7, test: 2, targets: vec![1, 4], score: 0 },
        Monkey { items: vec![69, 65, 63], op: |old| old + 6, test: 3, targets: vec![7, 2], score: 0 },
        Monkey { items: vec![89], op: |old| old * old, test: 11, targets: vec![0, 2], score: 0 },
    ];

    let mut monkeys = monkeys1;
    println!("part 1: {}", simulate_rounds(&mut monkeys.clone(), 20, &|worry| worry / 3));

    let tests_prod = monkeys.iter().fold(1, |p, m| p * m.test);
    println!("part 2: {}", simulate_rounds(&mut monkeys, 10_000, &|worry| worry % tests_prod));
}

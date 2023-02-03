use std::collections::LinkedList;

#[derive(Clone, Debug)]
struct Monkey {
    items: LinkedList<usize>,
    op: fn(usize) -> usize,
    test: usize,
    targets: Vec<usize>,
    score: usize,
}

fn simulate_rounds(ms: &mut Vec<Monkey>, rounds: usize, manage_worry: &dyn Fn(usize) -> usize) -> usize {
    for _ in 0..rounds {
        for i in 0..ms.len() {
            ms[i].score += ms[i].items.len();
            while let Some(item) = ms[i].items.pop_front() {
                let worry = manage_worry((ms[i].op)(item));
                let target = ms[i].targets[(worry % ms[i].test != 0) as usize];
                ms[target].items.push_back(worry);
            }
        }
    }

    ms.sort_by(|a, b| b.score.cmp(&a.score));

    if let [m1, m2] = &ms[0..2] {
        m1.score * m2.score
    } else {
        unreachable!();
    }
}

fn main() {
    let mut _monkeys0 = vec![
        Monkey { items: LinkedList::from([79, 98]), op: |old| old * 19, test: 23, targets: vec![2, 3], score: 0 },
        Monkey { items: LinkedList::from([54, 65, 75, 74]), op: |old| (old + 6), test: 19, targets: vec![2, 0], score: 0 },
        Monkey { items: LinkedList::from([79, 60, 97]), op: |old| old * old, test: 13, targets: vec![1, 3], score: 0 },
        Monkey { items: LinkedList::from([74]), op: |old| (old + 3), test: 17, targets: vec![0, 1], score: 0 },
    ];

    let monkeys1 = vec![
        Monkey { items: LinkedList::from([93, 98]), op: |old| old * 17, test: 19, targets: vec![5, 3], score: 0 },
        Monkey { items: LinkedList::from([95, 72, 98, 82, 86]), op: |old| old + 5, test: 13, targets: vec![7, 6], score: 0 },
        Monkey { items: LinkedList::from([85, 62, 82, 86, 70, 65, 83, 76]), op: |old| old + 8, test: 5, targets: vec![3, 0], score: 0 },
        Monkey { items: LinkedList::from([86, 70, 71, 56]), op: |old| old + 1, test: 7, targets: vec![4, 5], score: 0 },
        Monkey { items: LinkedList::from([77, 71, 86, 52, 81, 67]), op: |old| old + 4, test: 17, targets: vec![1, 6], score: 0 },
        Monkey { items: LinkedList::from([89, 87, 60, 78, 54, 77, 98]), op: |old| old * 7, test: 2, targets: vec![1, 4], score: 0 },
        Monkey { items: LinkedList::from([69, 65, 63]), op: |old| old + 6, test: 3, targets: vec![7, 2], score: 0 },
        Monkey { items: LinkedList::from([89]), op: |old| old * old, test: 11, targets: vec![0, 2], score: 0 },
    ];

    let mut monkeys = monkeys1;
    println!("part 1: {}", simulate_rounds(&mut monkeys.clone(), 20, &|worry| worry / 3));

    let tests_prod = monkeys.iter().fold(1, |p, m| p * m.test);
    println!("part 2: {}", simulate_rounds(&mut monkeys, 10_000, &|worry| worry % tests_prod));
}

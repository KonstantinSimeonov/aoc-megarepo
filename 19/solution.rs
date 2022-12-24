use std::fs;
use std::collections::HashMap;

// ore -> clay -> obsidian -> geodes

#[derive(Debug)]
struct BP {
    id: i32,
    ore_cost: i32,
    clay_cost: i32,
    obs_cost: (i32, i32),
    geode_cost: (i32, i32),

    max_ore: i32,
    max_clay: i32,
    max_obs: i32
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Income {
    ore: i32,
    clay: i32,
    obs: i32,
    geode: i32
}

impl Income {
    fn h(&self) -> i32 {
        self.ore.pow(4) + self.clay.pow(3) + self.obs.pow(2) + self.geode
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Bank {
    ore: i32,
    clay: i32,
    obs: i32,
    geode: i32
}

impl Bank {
    fn h(&self) -> i32 {
        self.ore.pow(4) + self.clay.pow(3) + self.obs.pow(2) + self.geode
    }
}

fn builds(bank: &Bank, bp: &BP, inc: &Income) -> Vec<(Income, Bank)> {
    let mut res = Vec::from([(inc.clone(), bank.clone())]);

    if bank.ore >= bp.ore_cost && inc.ore < bp.max_ore {
        let mut b = bank.clone();
        b.ore -= bp.ore_cost;
        let mut i = inc.clone();
        i.ore += 1;
        //println!("ore robot");
        res.push((i, b));
    }

    if bank.ore >= bp.clay_cost && inc.clay < bp.max_clay {
        let mut b = bank.clone();
        b.ore -= bp.clay_cost;
        let mut i = inc.clone();
        i.clay += 1;
        res.push((i, b));
    }

    if bank.ore >= bp.obs_cost.0 && bank.clay >= bp.obs_cost.1 && inc.obs < bp.max_obs {
        let mut b = bank.clone();
        b.ore -= bp.obs_cost.0;
        b.clay -= bp.obs_cost.1;
        let mut i = inc.clone();
        i.obs += 1;
        res.push((i, b));
    }

    if bank.ore >= bp.geode_cost.0 && bank.obs >= bp.geode_cost.1 {
        let mut b = bank.clone();
        b.ore -= bp.geode_cost.0;
        b.obs -= bp.geode_cost.1;
        let mut i = inc.clone();
        i.geode += 1;
        res.push((i, b));
    }

    res
}

fn main() {
    let input = fs::read_to_string("./input").expect("blueprints");

    let bps = input
        .lines()
        .map(|line| {
            let xs = line
                .split([' ', ':'])
                .map(|x| x.parse::<i32>())
                .filter(|x| x.is_ok())
                .map(|x| x.unwrap())
                .collect::<Vec<_>>();

            BP {
                id: xs[0],
                ore_cost: xs[1],
                clay_cost: xs[2],
                obs_cost: (xs[3], xs[4]),
                geode_cost: (xs[5], xs[6]),

                max_ore: *[xs[1], xs[2], xs[3], xs[5]].iter().max().unwrap(),
                max_clay: xs[4],
                max_obs: xs[6],
            }
        })
        .collect::<Vec<_>>();

    println!("{:?}", bps[0]);

    let bank = Bank { ore: 0, clay: 0, obs: 0, geode: 0 };
    let inc = Income { ore: 1, clay: 0, obs: 0, geode: 0 };

    // println!("{:?}", sim(inc, &bps[1], bank, 24, &mut HashMap::new()));

    //println!("{:?}", sim(&inc, &bps[1], &bank, 24, &mut HashMap::new()));

    let part1: i32 = bps
        .iter()
        .map(|bp| bp.id * sim(inc.clone(), bp, bank.clone(), 24, &mut HashMap::new()))
        .sum();

    println!("{:?}", part1)
}

fn sim(inc: Income, bp: &BP, bank: Bank, left: i32, cache: &mut HashMap<(i32, i32, i32), i32>) -> i32 {
    if let Some(ans) = cache.get(&(bank.h(), inc.h(), left)) {
        return *ans
    }

    if left <= 0 {
        //println!("{:?}", bank);
        return bank.geode
    }

    let bs = builds(&bank, bp, &inc);

    //println!("{:?} {:?} {:?} {}", inc, bank, bs, left);

    let ans = bs
        .into_iter()
        .map(|(i, mut b)| {
            b.ore += inc.ore;
            b.clay += inc.clay;
            b.obs += inc.obs;
            b.geode += inc.geode;

            sim(i, bp, b, left - 1, cache)
        })
        .max()
        .unwrap();

    cache.insert((bank.h(), inc.h(), left), ans);

    ans
}

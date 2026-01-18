use std::collections::HashSet;

use aoc2026::Inputs;

fn main() {
    let inputs = Inputs::read();
    solve(&inputs.test_inputs[0]);
    solve(&inputs.input);
}

type Box3D = [i64; 3];

fn solve(input: &str) {
    let boxes: Vec<Box3D> = input
        .lines()
        .map(|line| {
            let mut it = line.split(',').map(|x| x.parse::<i64>().unwrap());
            [it.next().unwrap(), it.next().unwrap(), it.next().unwrap()]
        })
        .collect();

    let mut pairs: Vec<(i64, usize, usize)> = vec![];
    for i in 0..boxes.len() - 1 {
        for j in i + 1..boxes.len() {
            pairs.push((dist_3d(&boxes[i], &boxes[j]), i, j))
        }
    }
    pairs.sort_unstable();

    let mut uf = UnionFind::new(boxes.len());
    let mut steps = 0;
    for &(_, x, y) in pairs.iter() {
        uf.union(x, y);
        steps += 1;

        if steps == 1000 {
            let mut sizes: Vec<usize> = uf
                .component_roots()
                .iter()
                .map(|root| uf.sizes[*root])
                .collect();
            sizes.sort_unstable_by(|x, y| y.cmp(x));

            let part1: usize = sizes.iter().take(3).product();
            println!("part 1: {}", part1);
        }

        if uf.is_fully_connected() {
            println!("part 2: {}", boxes[x][0] * boxes[y][0]);
            break;
        }
    }
}

struct UnionFind {
    parents: Vec<usize>,
    sizes: Vec<usize>,
    component_count: usize,
}

impl UnionFind {
    fn new(n: usize) -> Self {
        Self {
            parents: (0..n).collect(),
            sizes: vec![1; n],
            component_count: n,
        }
    }

    fn find_parent(&mut self, x: usize) -> usize {
        if self.parents[x] != x {
            self.parents[x] = self.find_parent(self.parents[x]);
        }

        self.parents[x]
    }

    fn union(&mut self, x: usize, y: usize) {
        let parent_x = self.find_parent(x);
        let parent_y = self.find_parent(y);

        if parent_x != parent_y {
            if self.sizes[parent_x] < self.sizes[parent_y] {
                self.parents[parent_x] = parent_y;
                self.sizes[parent_y] += self.sizes[parent_x];
            } else {
                self.parents[parent_y] = parent_x;
                self.sizes[parent_x] += self.sizes[parent_y];
            }
            self.component_count -= 1;
        }
    }

    fn is_fully_connected(&self) -> bool {
        self.component_count == 1
    }

    fn component_roots(&mut self) -> HashSet<usize> {
        let mut components: HashSet<usize> = HashSet::new();

        for i in 0..self.parents.len() {
            components.insert(self.find_parent(i));
        }

        components
    }
}

fn dist_3d(x: &Box3D, y: &Box3D) -> i64 {
    (0..3).map(|i| (x[i] - y[i]).pow(2)).sum()
}

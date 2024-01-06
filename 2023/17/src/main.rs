use std::cmp::Ordering;
use std::fs;
use std::collections::{BinaryHeap, HashMap};

fn main() {
  println!("{:?}", run_for_input("./input".to_owned()))
}

type V2 = (isize, isize);

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
struct State {
  cost: usize,
  pos: V2,
  dir: V2
}

impl Ord for State {
  fn cmp(&self, other: &State) -> Ordering {
    other.cost.cmp(&self.cost)
  }
}

impl PartialOrd for State {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct StateKey {
  pos: V2,
  dir: V2,
}

impl From<State> for StateKey {
  fn from(value: State) -> Self {
    Self {
      pos: value.pos,
      dir: value.dir,
    }
  }
}

fn get_next(State { cost, dir, pos }: State, input: &[&[u8]], dist_min: isize, dist_max: isize) -> Vec<State> {
  let mut stuff = vec![];
  for (dy, dx) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
    if (dy, dx) == dir || (-dy, -dx) == dir {
      continue
    }

    let mut nc = cost;
    for dist in 1..=dist_max {
      let ny = (pos.0 + dy * dist) as usize;
      let nx = (pos.1 + dx * dist) as usize;

      if ny >= input.len() || nx >= input[0].len() {
        break
      }

      nc += (input[ny][nx] - b'0') as usize;

      if dist < dist_min {
        continue
      }

      stuff.push(
        State {
          cost: nc,
          dir: (dy, dx),
          pos: (ny as isize, nx as isize)
        }
      )
    }
  }

  stuff
}

fn solve(input: &[&[u8]], dist_min: isize, dist_max: isize) -> usize {
  let end = ((input.len() - 1) as isize, (input[0].len() - 1) as isize);
  let mut nodes = BinaryHeap::from([State { cost: 0, pos: (0, 0), dir: (0, 0) }]);
  let mut visited = HashMap::<StateKey, usize>::new();

  while let Some(curr) = nodes.pop() {
    if curr.pos == end {
      return curr.cost
    }

    if visited.get(&curr.into()).is_some_and(|&cost| cost < curr.cost) {
      continue
    }

    for state in get_next(curr, &input, dist_min, dist_max) {
      if visited.get(&state.into()).is_some_and(|&cost| cost <= state.cost) {
        continue
      }

      visited.insert(state.into(), state.cost);
      nodes.push(state)
    }
  }

  unreachable!("fail")
}

fn run_for_input(path: String) -> (usize, usize) {
  let input = fs::read_to_string(path).expect("file to read");
  let result = input.trim().split("\n").map(str::as_bytes).collect::<Vec<_>>();
  (solve(&result, 1, 3), solve(&result, 4, 10))
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test0() {
    assert_eq!(run_for_input("./input0".to_owned()), (102, 94));
  }
}

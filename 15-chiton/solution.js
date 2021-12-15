const PriorityQueue = require(`js-priority-queue`)

const input0 = `
1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
`.trim()

const input = require(`fs`).readFileSync(`./input`, `utf8`).trim()

const parsed_grid = input.split(`\n`).map(l => l.split(``).map(Number))

const pretty = (g) => console.log(g.map(r => r.join(``)).join(`\n`), `\n`)

const solve = grid => {
  const nodes = new PriorityQueue({ initialValues: [{ x: 0, y: 0, risk: 0 }], comparator: (a, b) => a.risk - b.risk })
  const visited = grid.map(r => r.map(() => 0))
  while (nodes.length) {
    const { x, y, risk } = nodes.dequeue()
    const ds = [[1, 0], [-1, 0], [0, -1], [0, 1]]
      .map(([dx, dy]) => [x + dx, y + dy])
      .filter(([x, y]) => y < grid.length && 0 <= y && x < grid[y].length && 0 <= x)

    for (const [nx, ny] of ds) {
      const new_risk = visited[y][x] + grid[ny][nx]
      if (!visited[ny][nx] || visited[ny][nx] > new_risk) {
        nodes.queue({ x: nx, y: ny, risk: new_risk })
        visited[ny][nx] = risk + grid[ny][nx]
      }
    }
  }

  return visited
}

const x5 = grid => {
  const { length: l1 } = grid
  const { length: l2 } = grid[0]
  const xs = Array.from({ length: l1 * 5 }, () => Array.from({ length: l2 * 5 }))

  for (let i = 0; i < xs.length; ++i) {
    for (let j = 0; j < xs[i].length; ++j) {
      const step = (i / l1 | 0) + (j / l2 | 0)
      xs[i][j] = (grid[i % l1][j % l2] + step)
      xs[i][j] = xs[i][j] > 9 ? xs[i][j] % 9 : xs[i][j]
    }
  }

  return xs
}

console.log({ part1: solve(parsed_grid).pop().pop() })
console.log({ part2: solve(x5(parsed_grid)).pop().pop() })

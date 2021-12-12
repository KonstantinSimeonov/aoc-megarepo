const input = `5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526`

const input00 = `11111
19991
19191
19991
11111`

const input1 = `6318185732
1122687135
5173237676
8754362612
5718474666
8443654137
1247634346
1446514585
6717288267
1727871228`

const grid = input1.split(`\n`).map(row => row.split(``).map(Number))

const pad = x => {
  const y = x === 0 ? `\x1b[1;34;22m${x}\x1b[0m` : String(x)
  return x < 10 ? `  ${y}` : ` ${y}`
}

const pretty_grid = (gr, ...args) => console.log(...args, gr.map(row => row.map(pad).join(``)).join(`\n`), `\n`)

const ds = [[0, 1], [0, -1], [1, 0], [-1, 0], [1, 1], [-1, 1], [-1, -1], [1, -1]]
const is_inside = gr => ([x, y]) => gr[y] && gr[y][x] !== undefined

const flash = (gr, x, y, visited = new Set) => {
  if (gr[y][x] < 10) return
  if (visited.has(`${x} ${y}`)) return
  visited.add(`${x} ${y}`)

  const adj = ds.map(([dx, dy]) => [x + dx, y + dy]).filter(is_inside(gr))
  for (const [nx, ny] of adj) {
      ++gr[ny][nx]
      flash(gr, nx, ny, visited)
  }
}
const step = gr => {
  for (let y = 0; y < gr.length; ++y) {
    for (let x = 0; x < gr[y].length; ++x) {
      ++gr[y][x]
    }
  }

  const visited = new Set
  for (let y = 0; y < gr.length; ++y) {
    for (let x = 0; x < gr[y].length; ++x) {
      flash(gr, x, y, visited)
    }
  }

  let flashes = 0
  for (let y = 0; y < gr.length; ++y) {
    for (let x = 0; x < gr[y].length; ++x) {
      if (gr[y][x] >= 10) {
        ++flashes
        gr[y][x] = 0
      }
    }
  }

  return flashes
}

let total_flashes = 0
for (let i = 1; i < 300; ++i) {
  const new_f = step(grid)
  total_flashes += new_f
  if (i === 100) {
    console.log({ part1: total_flashes })
  }

  if (grid.flat(1).every(x => x === 0)) {
    console.log({ part2: i })
    break
  }
}

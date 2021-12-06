const input = require(`fs`).readFileSync(`./input`, `utf8`)

const hv = input
  .trim()
  .split(`\n`)
  .map(
    l => l.split(` -> `)
      .map(p => p.split(`,`).map(Number))
  )
  // uncomment for part 1
  //.filter(([[fx, fy], [tx, ty]]) => fx === tx || fy === ty)

const length = Math.max(...hv.flat(Infinity)) + 1
const grid = Array.from({ length }, () => Array.from({ length }, () => 0))

const pretty_grid = g => `\n${g.map(r => r.map(x => x ? String(x) : `.`).join(``)).join(`\n`)}\n`

for (const [[fx, fy], [tx, ty]] of hv) {
  let dx = tx - fx
  let dy = ty - fy
  grid[fy][fx] += 1
  while (dx || dy) {
    grid[fy + dy][fx + dx] += 1
    dx = dx ? dx - Math.sign(dx) : dx
    dy = dy ? dy - Math.sign(dy) : dy
  }
}


const result = grid.flat().filter(x => x > 1).length

console.log(result)

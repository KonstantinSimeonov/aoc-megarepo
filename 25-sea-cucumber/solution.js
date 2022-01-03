const to_grid = str => str.trim().split(`\n`)

const rotate = xs => {
  const ys = []
  for (let j = 0; j < xs[0].length; ++j) {
    ys[j] = []
    for (let i = 0; i < xs.length; ++i) {
      ys[j][i] = { '.': '.', 'v': '>', '>': 'v' }[xs[i][j]]
    }
  }

  return ys.map(y => y.join(``))
}

const move = line => {
  const r = line.replace(/>\./g, `.>`)
  if (line[0] === `.` && line[r.length - 1] === `>`)
    return `>${r.slice(1, -1)}.`
  return r
}

const eq_grid = (xs, ys) => xs.every((x, i) => x === ys[i])

const p = xs => console.log(xs.join(`\n`), `\n`)

const step = xs => rotate(
  rotate(
    xs.map(move)
  ).map(move)
)

const grid = to_grid(require(`fs`).readFileSync(`./input`, `utf8`))

const solve = (i, g) => {
  const ng = step(g)
  return eq_grid(g, ng) ? 1 : 1 + solve(i, ng)
}

console.log(solve(0, grid))

const params = require(`fs`)
  .readFileSync(`./input`, `utf8`)
  .trim()
  .split(`inp w\n`)
  .slice(1)
  .map(ls => {
    const s = ls.split(`\n`)
    return [s[3], s[4], s[14]].map(x => +x.split(` `).pop())
  })

const valid = []

const solve = (params, stack, number) => {
  if (params.length === 0) {
    if (stack === 0) valid.push(number)

    return
  }

  const [[d, add, elem], ...rest] = params

  const x = stack % 26
  stack = stack / d | 0

  for (let w = 9; w >= 1; --w) {
    if (add > 0)
      solve(rest, stack * 26 + w + elem, number * 10 + w)
    else if (add + x === w)
      solve(rest, stack, number * 10 + w)
  }
}

solve(params, 0, 0)

console.log(
`part 1: ${valid[0]}
part 2: ${valid[valid.length - 1]}`
)

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

const solve = (params, stack, path) => {
  if (params.length === 0) {
    if (stack.length === 0) valid.push(path.join(``))

    return
  }

  const [[d, add, elem], ...rest] = params

  const x = (stack[stack.length - 1] || 0) % 26
  if (d === 26) stack.pop()

  for (let w = 9; w >= 1; --w) {
    if (add > 0)
      solve(rest, [...stack, w + elem], [...path, w])
    else if (add + x === w)
      solve(rest, stack, [...path, w])
  }
}

solve(params, [], [])

console.log(
`part 1: ${valid[0]}
part 2: ${valid[valid.length - 1]}`
)

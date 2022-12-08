const input = require(`fs`)
  .readFileSync(`./input`, `utf-8`)

const closing_count =
  input.match(/\$ cd .+/g).length - 2 * input.match(/\$ cd \.\./g).length

const func_str = input
  .replace(/\$ ls/g, ``)
  .replace(/\$ cd \.\./g, `],`)
  .replace(/dir .+/g, ``)
  .replace(/([0-9]+) .+/g, `$1,`)
  .replace(/\$ cd .+/g, `[`)
  + `]`.repeat(closing_count)

const dfs = arr => [
  arr.flat(Infinity).reduce((a, b) => a + b),
  arr.filter(Array.isArray).map(dfs)
].flat(Infinity)

const sizes = dfs(eval(func_str))
const part1 = sizes.filter(s => s <= 100_000).reduce((a, b) => a + b)
const part2 = Math.min(...sizes.filter(s => 40_000_000 >= sizes[0] - s))

console.log({ part1, part2 })

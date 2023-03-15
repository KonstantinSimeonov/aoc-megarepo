const input = require(`fs`)
  .readFileSync(`./input`, `utf-8`)

const closing_count =
  input.match(/\$ cd .+/g).length - 2 * input.match(/\$ cd \.\./g).length

const array_str = input
  .replace(/\$ ls/g, ``) // ls is useless
  .replace(/\$ cd \.\./g, `],`) // close the array
  .replace(/dir .+/g, ``) // dir is useless
  .replace(/([0-9]+) .+/g, `$1,`) // parse the file into a number member
  .replace(/\$ cd .+/g, `[`) // open the array bracket
  + `]`.repeat(closing_count) // need to close them brackets

const calc_all_sums = arr => [
  arr.flat(Infinity).reduce((a, b) => a + b),
  arr.filter(Array.isArray).map(calc_all_sums)
].flat(Infinity)

const sizes = calc_all_sums(eval(array_str))
const part1 = sizes.filter(s => s <= 100_000).reduce((a, b) => a + b)
const part2 = Math.min(...sizes.filter(s => 40_000_000 >= sizes[0] - s))

console.log({ part1, part2 })

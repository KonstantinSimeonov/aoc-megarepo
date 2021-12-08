const input = require(`fs`).readFileSync(`./input`, `utf8`).trim()

const contains = ys => xs => [].every.call(ys, y => xs.includes(y))
const permutes = ys => xs => ys.length === xs.length && contains(ys)(xs)

const group_by = (xs, keyfn) => xs.reduce(
  (grs, x) => {
    const key = keyfn(x)
    return { ...grs, [key]: [...(grs[key] || []), x] }
  },
  {}
)

const sort_by_length = xs => xs.sort((a, b) => a.length - b.length)

const decode = encoding => {
  // 1,4,7,8 have unique segments count, the rest don't
  const { true: _1478, false: _023569 } = group_by(encoding, x => contains([x.length])([2, 3, 4, 7]))
  // 1 -> 2 segments, 7 -> 3 segments, 4 -> 4 segments, 8 -> 7 segments
  const [_1, _7, _4, _8] = sort_by_length(_1478)
  // 3,0,9 all contain 1 in their pattern, the rest don't
  const { true: _309, false: _256 } = group_by(_023569, contains(_1))
  // 3 has 5 segments, 0 and 9 have 6
  const [_3, ..._09] = sort_by_length(_309)
  // 9 contains the pattern of 3, 0 doesn't
  const { true: [_9], false: [_0] } = group_by(_09, contains(_3))
  // 6 has the 6 segments, 2 and 5 have 5
  const [_6, ..._25] = sort_by_length(_256).reverse()
  // 6 contains the pattern for 5, 2 doesn't
  const { true: [_5], false: [_2] } = group_by(_25, x => contains(x)(_6))

  return [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9]
}

const result = input
  .split(`\n`)
  .reduce((sum, line) => {
    const [encoding, output] = line.split(` | `).map(nums => nums.split(` `))
    const decoded_digits = decode(encoding)
    const decoded_output = output.map(o => decoded_digits.findIndex(permutes(o)))
    return sum + Number(decoded_output.join(``))
  }, 0)

console.log(result)

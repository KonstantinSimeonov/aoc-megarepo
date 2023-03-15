const input = require(`fs`).readFileSync(`./input`, `utf8`).trim()

const group_by = (keyfn, xs) => xs.reduce(
  (grs, x) => {
    const key = keyfn(x)
    return { ...grs, [key]: [...(grs[key] || []), x] }
  },
  {}
)

const hash_decode_output = (() => {
  const hash = xs => [...xs].reduce((prod, x) => prod * (x.charCodeAt(0) - 70), 1)
  const divisible_by = h1 => h2 => h2 % h1 === 0

  const decode_hash = encoding => {
    const [h1, h7, h4, ...h0235689] = encoding.map(hash).sort((ha, hb) => ha - hb)
    const h8 = h0235689.slice(-1)[0]

    const h069 = h0235689.slice(3, 6)
    const { true: h09, false: [h6] } = group_by(divisible_by(h1), h069)
    const { true: [h9], false: [h0] } = group_by(divisible_by(h4), h09)

    const h253 = h0235689.slice(0, 3)
    const { true: [h3], false: h25 } = group_by(divisible_by(h1), h253)
    const { true: [h5], false: [h2] } = group_by(x => divisible_by(x)(h6), h25)

    return [h0, h1, h2, h3, h4, h5, h6, h7, h8, h9]
  }

  return (encoding, output) => {
    const encoding_hashes = decode_hash(encoding)
    const decoded_output = output.map(o => encoding_hashes.indexOf(hash(o)))
    return Number(decoded_output.join(``))
  }
})()

const decode_output = (() => {
  const contains = ys => xs => [].every.call(ys, y => xs.includes(y))
  const permutes = ys => xs => ys.length === xs.length && contains(ys)(xs)
  const sort_by_length = xs => xs.sort((a, b) => a.length - b.length)

  const decode = encoding => {
    // 1,4,7,8 have unique segments count, the rest don't
    const { true: _1478, false: _023569 } = group_by(x => contains([x.length])([2, 3, 4, 7]), encoding)
    // 1 -> 2 segments, 7 -> 3 segments, 4 -> 4 segments, 8 -> 7 segments
    const [_1, _7, _4, _8] = sort_by_length(_1478)
    // 3,0,9 all contain 1 in their pattern, the rest don't
    const { true: _309, false: _256 } = group_by(contains(_1), _023569)
    // 3 has 5 segments, 0 and 9 have 6
    const [_3, ..._09] = sort_by_length(_309)
    // 9 contains the pattern of 3, 0 doesn't
    const { true: [_9], false: [_0] } = group_by(contains(_3), _09)
    // 6 has the 6 segments, 2 and 5 have 5
    const [_6, ..._25] = sort_by_length(_256).reverse()
    // 6 contains the pattern for 5, 2 doesn't
    const { true: [_5], false: [_2] } = group_by(x => contains(x)(_6), _25)

    return [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9]
  }

  return (encoding, output) => {
    const decoded_digits = decode(encoding)
    const decoded_output = Number(output.map(o => decoded_digits.findIndex(permutes(o))).join(``))
    return decoded_output
  }
})()

const part2 = strategy => {
  const result = input
    .split(`\n`)
    .reduce(
      (sum, line) => sum + strategy(...line.split(` | `).map(nums => nums.split(` `))),
      0
    )
  console.log(result)
}

part2(decode_output)
part2(hash_decode_output)

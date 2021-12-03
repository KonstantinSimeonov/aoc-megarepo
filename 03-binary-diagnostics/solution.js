const input = require(`fs`).readFileSync(`./input`).toString().trim()

const lines = input.split(`\n`)

const solve = () => {
  const sums = new Uint32Array(12)
  const lines_length = lines.length
  const half = lines_length >> 1
  const sums_length = sums.length
  for (let i = 0; i < lines_length; ++i)
    for (let j = 0; j < sums_length; ++j)
      sums[j] += +lines[i][j]

  let eps = 0
  let gam = 0
  for (let i = 0; i < sums_length; ++i) {
    eps <<= 1
    gam <<= 1
    const gt = sums[i] >= half
    eps += gt
    gam += !gt
  }

  return { eps, gam, result: eps * gam }
}

(() => {
  const start = Date.now()
  const answer = solve()
  const time = Date.now() - start
  console.log(`part 1: `, { ...answer, time })
})()

const find_most = (ls, i = 0) => {
  if (ls.length <= 1) return parseInt(ls[0], 2)

  const bit = ls[ls.length >> 1][i]
  const most = ls.filter(x => x[i] === bit)
  return find_most(most, i + 1)
}

const find_least = (ls, i = 0) => {
  if (ls.length <= 1) return parseInt(ls[0], 2)

  const bit = ls[ls.length >> 1][i] ^ 1
  const least = ls.filter(x => x[i] == bit)
  return find_least(least, i + 1)
}

(() => {
  let now = Date.now()
  lines.sort()
  let co2 = find_least(lines)
  let oxygen = find_most(lines)
  let answer = co2 * oxygen
  let time = Date.now() - now
  console.log(`part 2: `, { co2, oxygen, answer, time })
})()

const input = require(`fs`).readFileSync(`./input`).toString().trim()

const lines = input.split(`\n`)

;(() => {
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
      eps = eps * 2 + (sums[i] >= half)
      gam = gam * 2 + (sums[i] < half)
    }

    return { eps, gam, result: eps * gam }
  }

  const start = Date.now()
  const answer = solve()
  const time = Date.now() - start
  console.log(`part 1: `, { ...answer, time })
})()

;(() => {
  const find = (ls, get_bit, i = 0) => {
    if (ls.length <= 1) return parseInt(ls[0], 2)

    const bit = get_bit(ls, i)
    const next_ls = ls.filter(x => x[i] == bit)
    return find(next_ls, get_bit, i + 1)
  }

  const now = Date.now()
  lines.sort()
  const co2 = find(lines, (ls, i) => ls[ls.length >> 1][i] ^ 1)
  const oxygen = find(lines, (ls, i) => ls[ls.length >> 1][i])
  const answer = co2 * oxygen
  const time = Date.now() - now
  console.log(`part 2: `, { co2, oxygen, answer, time })
})()

const rolls = [[1,3],[3,4],[6,5],[7,6],[6,7],[3,8],[1,9]]

const key = (a, b, c, d) => (b * 10000) + (c * 1000) + (a * 100) + d
const play = (p1, s1, p2, s2, mem) => {
  const k = key(p1, s1, p2, s2)
  if (s2 >= 21) return mem[k] = [0, 1]
  if (mem[k]) return mem[k]

  let sw1 = 0
  let sw2 = 0
  for (const [l, r] of rolls) {
    const roll_p = p1 + (r % 10)
    const new_position1 = roll_p > 10 ? roll_p % 10 : roll_p

    const [w2, w1] = play(p2, s2, new_position1, s1 + new_position1, mem)
    sw1 += w1 * l
    sw2 += w2 * l
  }

  return mem[k] = [sw1, sw2]
}

console.log(play(10, 0, 4, 0, {}).sort((a, b) => b - a))

const input1 = require(`fs`).readFileSync(`./input`, `utf8`)
const input = input1.split(`\n`).map(x => Array.from(x, Number))

const ds = [
  [0, -1],
  [0, 1],
  [1, 0],
  [-1, 0]
]

const is_low = (ascii, rows, y, x) =>
  ds.map(([dx, dy]) => [x + dx, y + dy])
    .every(
      ([nx, ny]) => !rows[ny] || (rows[ny][nx] === undefined) || rows[ny][nx] > ascii
    )


const [part1, points]  = input.reduce(
  ([sum, all_lows], row, y, rows) => {
    const lows = row
      .map((ascii, x) => [is_low(ascii, rows, y, x), ascii, [x, y]])
      .filter(x => x[0])

    const sum_lows = lows.reduce((row_sum, [, value]) => row_sum + value + 1, 0)
    return [sum + sum_lows, [...all_lows, ...lows.map(l => l[2])]]
  },
  [0, []]
)

console.log(part1)

const count_basin_for_point = rows => ([x, y]) => {
  if (rows[y][x] >= 9) return 0
  rows[y][x] = 9

  const sum = ds
    .map(([dx, dy]) => [x + dx, y + dy])
    .filter(([nx, ny]) => rows[ny] && rows[ny][nx] && rows[ny][nx] < 9)
    .map(count_basin_for_point(rows))
    .reduce((sum, x) => sum + x, 0)

  return 1 + sum
}

const [a, b, c] = points.map(count_basin_for_point(input)).sort((a, b) => b - a)

console.log(a * b * c)

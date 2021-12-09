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
    const lows = row.map(
      (ascii, x) => [is_low(ascii, rows, y, x), ascii, [x, y]]
    ).filter(x => x[0])


    const sum_lows = lows.reduce((row_sum, [, value]) => row_sum + value + 1, 0)
    return [sum + sum_lows, [...all_lows, ...lows.map(l => l[2])]]
  },
  [0, []]
)

console.log(part1)

const count_basin_for_point = (y, x, rows) => {
  if (rows[y][x] >= 9) return 0

  const next = ds.map(([dx, dy]) => [x + dx, y + dy])
    .filter(
      ([nx, ny]) => rows[ny] && rows[ny][nx] && rows[ny][nx] !== 9
    )
  rows[y][x] = 9
  const s = next.map(([nx, ny]) => count_basin_for_point(ny, nx, rows)).reduce(
    (sum, x) => sum + x, 0
  )

  return 1 + s
}

const [a, b, c] = points.map(
  ([x, y]) => count_basin_for_point(y, x, input)
).sort((a, b) => b - a).slice(0, 3)
console.log(a * b * c)

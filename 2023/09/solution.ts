declare const require: (m: `fs`) => {
  readFileSync(path: string, charset: `utf-8`): string
}

const read = (path: string): number[][] =>
  require(`fs`)
    .readFileSync(path, `utf-8`)
    .trim()
    .split(`\n`)
    .map(line => line.split(` `).map(Number))

type Hist = readonly number[]
type Triangle = readonly Hist[]

const diffs = (xs: Hist): Hist =>
  xs.slice(0, -1).map((x, i) => xs[i + 1] - x)

const triangle = (h: Hist): Triangle => {
  const next = diffs(h)
  return next.some(x => x !== 0) ? [h, ...triangle(next)] : [h, next]
}

const last = <T>(xs: readonly T[]): T => xs[xs.length - 1]

const sum = (xs: Hist) => xs.reduce((a, b) => a + b, 0)

const up_left = (hs: Triangle): number =>
  hs.reduce(
    (sum, h) => sum + last(h),
    0
  )

const up_right = (hs: Triangle): number =>
  hs.reduceRight(
    (sum, h) => h[0] - sum,
    0
  )

const solve = (path: string) => {
  const lasts = read(path).map(triangle)

  const part1 = sum(lasts.map(up_left))
  const part2 = sum(lasts.map(up_right))

  console.log({ part1, part2 })
}

solve(`./input0`)
solve(`./input`)

const input = require(`fs`).readFileSync(`./input`, `utf8`).trim()

const intersect_line = (a1, a2, b1, b2) =>
  b2 < a1 || b1 > a2
    ? []
    : [Math.max(a1, b1), Math.min(a2, b2)]

const volume = c =>
  c[0]
  * (c[2] - c[1] + 1)
  * (c[4] - c[3] + 1)
  * (c[6] - c[5] + 1)

const intersect = (a, b, sign) => [
  sign,
  ...intersect_line(a[1], a[2], b[1], b[2]),
  ...intersect_line(a[3], a[4], b[3], b[4]),
  ...intersect_line(a[5], a[6], b[5], b[6])
]

const solve = inp => inp
  .replace(/on/g, `1`)
  .replace(/off/g, `-1`)
  .split(`\n`)
  .map(l => l.match(/-?\d+/g).map(Number))
  .reduce((cuboids_so_far, c) => {
    const intersections = cuboids_so_far
      .map(x => intersect(c, x, -x[0]))
      .filter(is => is.length === 7)

    return c[0] === 1
      ? [...cuboids_so_far, ...intersections, c]
      : [...cuboids_so_far, ...intersections]
  }, [])
  .reduce((count, cuboid) => count + volume(cuboid), 0)

console.log(solve(input))

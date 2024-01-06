declare const require: (m: `fs`) => {
  readFileSync(path: string, charset: `utf-8`): string
}

const read = (path: string) =>
  require(`fs`)
    .readFileSync(path, `utf-8`)
    .trim()
    .split(`\n\n`)

const to_js = (map: string) => `(() => {
  ${map
  .replace(/(^|\n)/g, `\n  const `)
  .replace(/\(/g, `dir => dir ? `)
  .replace(/,/g, ` :`)
  .replace(/\)/g, `;`)}

  return {
    ${map.match(/([A-Z]{2}[A])(?= \=)/g)?.join(`,\n    `)}
  }
})()
`

const cycle = (instructions: string) => {
  let i = 0;

  return (): boolean => {
    const dir = instructions[i] === `L`
    i = (i + 1) % instructions.length
    return dir
  }
}

type DirFn = (fn: boolean) => DirFn
type DirMap = Record<`${string}A`, DirFn>

const run_to_z = (start: DirFn, next_dir: () => boolean) => {
  let steps = BigInt(0)
  do {
    ++steps
    start = start(next_dir())
  } while (!start.name.endsWith(`Z`));

  return { steps, next_dir, start }
}

const gcd = (a: bigint, b: bigint): bigint =>
  b === BigInt(0) ? a : gcd(b, a % b)

const lcm = (a: bigint, b: bigint): bigint =>
  a * b / gcd(a, b)

const pick = <X extends string, T extends Record<X, any>>(x: T, ...keys: readonly X[]): Pick<T, X> =>
  Object.assign({}, ...keys.map(key => ({ [key]: x[key] })))

const solve = (path: string) => {
  const [instructions, map] = read(path)

  console.log(to_js(map))

  const js_map: DirMap = eval(to_js(map))

  const part1 = run_to_z(js_map.AAA, cycle(instructions))

  console.log(js_map)
  const ps = Object.values(js_map).map(
    s => run_to_z(s, cycle(instructions))
  )

  const part2 = ps.map(p => p.steps).reduce(lcm)

  console.log({ part1, part2 })
}

//solve(`./input0`)
//solve(`./input1`)
solve(`./input2`)
//solve(`./input`)

//const explore_cycle = (start: DirFn, next_dir: () => boolean) => {
//  const visited = new Set<string>()
//
//  let steps = BigInt(0)
//
//  const st: { steps: BigInt, from: DirFn, to: DirFn }[] = []
//
//  for (let i = 0; i < 20; ++i) {
//    visited.add(start.name)
//    const next_z = run_to_z(start, next_dir)
//    st.push({ steps: next_z.steps, from: start, to: next_z.start })
//    steps += next_z.steps
//    start = next_z.start
//  }
//
//  return { start, visited, steps, st }
//}

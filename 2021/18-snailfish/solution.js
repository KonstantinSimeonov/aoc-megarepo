`use-strict`

const pipe = (...fns) => fns.reduceRight((fn, x) => (...args) => fn(x(...args)))

const render_tree = node =>
  `value` in node
    ? `${node.value}`
    : `[${render_tree(node[0])},${render_tree(node[1])}]`

const render = pipe(render_tree, console.log)

const reduce_number = num => {
  const to_tree = (value, garmeji, i = 0, parent = null, depth = 0) => {
    if (typeof value === `number`) return { value, i, depth, parent }

    const node = { parent, depth, i }
    Object.assign(node, value.map((v, i) => to_tree(v, garmeji, i, node, depth + 1)))
    if (depth === 4) garmeji.push(node)
    return node
  }

  const sibling = (dir, node) => {
    let p = node
    if (node.i === dir)
      while (p && p.i === dir)
        p = p.parent
    else
      while (p && !p[dir])
        p = p.parent

    if (!p || !p.parent) return

    let s = p.parent[dir]
    while (s[dir ^ 1] || s[dir])
      s = s[dir ^ 1] || s[dir]

    return s
  }

  const garmim_umereno = node => {
    const left = sibling(0, node)
    if (left) left.value += node[0].value
    const right = sibling(1, node)
    if (right) right.value += node[1].value

    node.parent[node.i] = { value: 0, i: node.i, depth: node.depth, parent: node.parent }
  }

  const find_split = n =>
    `value` in n
      ? (n.value > 9 ? n : null)
      : (find_split(n[0]) || find_split(n[1]))

  const qko_garmej = []
  const tree = to_tree(eval(num), qko_garmej)

  qko_garmej.forEach(garmim_umereno)

  while (true) {
    const s = find_split(tree)
    if (!s) return render_tree(tree)

    const parent = { i: s.i, depth: s.depth, parent: s.parent }
    s.parent[s.i] = parent
    const pair_nodes = [s.value >> 1, (s.value + 1) >> 1]
      .map((value, i) => ({ value, i, depth: s.depth + 1, parent }))
    Object.assign(parent, pair_nodes)

    if (s.depth >= 4) {
      garmim_umereno(s.parent[s.i])
    }
  }
}

const input = require(`fs`)
  .readFileSync(`./input`, `utf8`)
  .trim()
  .split(`\n`)

const add = (x, y) => `[${x},${y}]`

const sum = input.reduce(pipe(add, reduce_number))

const magnitude = node =>
  typeof node === `number`
    ? node
    : 3 * magnitude(node[0]) + 2 * magnitude(node[1])

console.log(`part 1:`, magnitude(eval(sum)))

const all_sums = input.flatMap(x => input.flatMap(y => [add(x, y), add(y, x)]))
const part2 = Math.max(
  ...all_sums.map(
    pipe(reduce_number, eval, magnitude)
  )
)

console.log(`part 2:`, part2)

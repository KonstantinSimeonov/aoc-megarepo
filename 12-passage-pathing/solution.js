const solve = input => {
  const mesh = input.split(`\n`).reduce(
    (m, pair) => {
      const [from, to] = pair.split(`-`)
      return {
        ...m,
        [from]: [...(m[from] || []), to],
        [to]: [...(m[to] || []), from]
      }
    },
    {}
  )

  const visited = {}
  const visit = x => visited[x] = (visited[x] || 0) + 1
  const unvisit = x => visited[x]--
  const can_visit = (x, visited) => {
    if (!visited[x]) return true
    if (x === `start`) return false
    for (const key in visited) {
      if (visited[key] >= 2) return false
    }
    return true
  }

  let paths_count = 0

  const calc_paths = from => {
    if (from === `end` || !mesh[from]) {
      ++paths_count
      return
    }

    if (!can_visit(from, visited)) return

    if (from.toLowerCase() === from) {
      visit(from)
      mesh[from].forEach(calc_paths)
      unvisit(from)
    } else
      mesh[from].forEach(calc_paths)
  }

  calc_paths(`start`)

  return paths_count
}

const puzzle = `mj-TZ
start-LY
TX-ez
uw-ez
ez-TZ
TH-vn
sb-uw
uw-LY
LY-mj
sb-TX
TH-end
end-LY
mj-start
TZ-sb
uw-RR
start-TZ
mj-TH
ez-TH
sb-end
LY-ez
TX-mt
vn-sb
uw-vn
uw-TZ`

console.log(solve(puzzle))

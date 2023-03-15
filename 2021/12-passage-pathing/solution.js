const solve = input => {
  const is_lower = xs => xs.charCodeAt(0) >= 97

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
  const can_visit = x => {
    if (!visited[x]) return true
    if (x === `start`) return false
    for (const key in visited)
      if (visited[key] >= 2) return false

    return true
  }

  let paths_count = 0

  const calc_paths = from => {
    if (from === `end`) {
      ++paths_count
      return
    }

    if (!can_visit(from)) return

    const lower = is_lower(from)
    lower && visit(from)
    mesh[from].forEach(calc_paths)
    lower && unvisit(from)
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

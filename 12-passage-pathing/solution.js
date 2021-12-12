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

  const visit = (x, visited) => x.toLowerCase() === x ? { ...visited, [x]: (visited[x] || 0) + 1 } : visited
  const can_visit = (x, visited) => {
    if (!(x in visited)) return true
    if (x === `start`) return false
    for (const key in visited) {
      if (visited[key] >= 2) return false
    }
    return true
  }

  const ps = []

  const calc_paths = (from, visited, path) => {
    if (from === `end` || !mesh[from]) {
      ps.push([...path, from])
      return
    }

    if (!can_visit(from, visited)) {
      return
    }

    mesh[from].forEach(to => calc_paths(to, visit(from, visited), [...path, from]))
  }

  calc_paths(`start`, {}, [])

  return ps
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

console.log(solve(puzzle).length)

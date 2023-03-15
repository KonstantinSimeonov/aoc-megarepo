const input = require(`fs`).readFileSync(`./input1`, `utf8`)

const stuff = input.split(`\n\n`)

const numbers = stuff[0].split(`,`).map(Number)
const boards = stuff
  .slice(1)
  .map(
    board_string => {
      const rows = board_string.trim().split(`\n`).map(
          r => r.split(` `).filter(Boolean).map(Number)
      )
      const cols = rows.map(
        (row, i) => row.map((_, j) => rows[j][i])
      )
      return [...rows, ...cols];
    }
  )

const solve = (current_boards, i, won_boards) => {
  if (i >= numbers.length) return won_boards

  const n = numbers[i]
  const new_boards = current_boards.map(
    board => board.map(rc => rc.filter(x => x !== n))
  )
  const new_won = new_boards
    .filter(board => board.some(rc => rc.length === 0))
    .map(board => n * board.flat().reduce((a, b) => a + b) / 2)
  const next_boards = new_boards.filter(board => board.every(rc => rc.length > 0))
  return solve(next_boards, i + 1, [...won_boards, ...new_won])
}

console.log(solve(boards, 0, []))

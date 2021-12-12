const canvas = document.querySelector(`canvas`)
const label = document.querySelector(`#step`)

const S = 20

Object.assign(canvas, { width: grid[0].length * S, height: grid.length * S })

const ctx = canvas.getContext(`2d`)

;(async () => {
  const sleep = ms => new Promise(r => setTimeout(r, ms))

  const rgb = (...rgb) => `rgb(${rgb.map(x => x | 0).join(`, `)})`

  const render = () => {
    for (let y = 0; y < grid.length; ++y) {
      for (let x = 0; x < grid[y].length; ++x) {
        const e = grid[y][x] / 9
        ctx.fillStyle = rgb(128 * e, 255 * e, 255 * e)
        ctx.fillRect(x * S, y * S, S, S)
      }
    }
  }

  for (let i = 0; i < 1000; ++i) {
    step(grid)
    render()
    label.innerText = i
    await sleep(100)
  }
})()

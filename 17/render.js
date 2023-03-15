const render = () => {
  const c = document.querySelector(`canvas`)
  const dr = c.getContext(`2d`)

  const l = 6_000
  const w = c.width / l

  for (let i = 1; i < l; ++i) {
    dr.beginPath()
    dr.moveTo(i * w, 0)
    dr.lineTo(i * w, (data[i] - data[i - 1]) * 5)
    dr.fill()
    dr.stroke()
    dr.closePath()
  }
}

document.addEventListener(`DOMContentLoaded`, render)

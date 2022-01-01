window.onload = () => {
  const input = `               
   ........... 
     B A B C   
     D C B A   
     D B A C   
     D A D C   
               `

  const mk_elem = (name, attrs = {}) => Object.assign(
    document.createElement(name),
    attrs
  )

  const rows = input
    .split(`\n`)
    .map(
      (r, i) => `<div>${
        Array.from(
          r,
          (x, j) => `<button data-r="${i}" data-c="${j}" style="width: 20px; height: 20px; visibility: ${x === ` ` ? `hidden` : `visible`}">${x === ` ` ? `&nbsp;` : x}</button>`
        )
        .join(``)
      }</div>`
    )

  const grid = `<div>${rows.join(``)}</div>`

  document.body.innerHTML += grid

  const s = mk_elem(`input`, { disabled: true, value: 0 })
  document.body.appendChild(s)

  const hist = mk_elem(`ol`)
  document.body.appendChild(hist)

  const to_hist = ({ innerHTML, dataset: { r, c } }, { dataset }) => `<li>${innerHTML} (${r},${c}) -> (${dataset.r},${dataset.c})`

  let selected
  let sum = 0
  const COST = { A: 1, B: 10, C: 100, D: 1000 }
  document.body.addEventListener(`click`, event => {
    if (event.target.tagName !== `BUTTON`) return

    const btn = event.target
    if (selected === btn) {
      btn.style.border = ``
      selected = undefined
      return
    }

    if (!selected) {
      selected = btn
      btn.style.border = `1px solid red`
      return
    }

    if (selected.dataset.r == 1 && btn.dataset.r == 1) return console.error(`can't stay on row`)
    if (selected.dataset.r == 1 && btn.dataset.r == 1) return console.error(`can't stay on row`)
    if (btn.dataset.r == 1 && [5, 7, 9, 11].includes(+btn.dataset.c)) return console.error(`can't stand in front of door`)
    hist.innerHTML += to_hist(selected, btn)

    const x = selected.innerHTML
    selected.innerHTML = `.`
    btn.innerHTML = x
    const e = COST[x]
    const dy = Math.abs(btn.dataset.r - selected.dataset.r)
    const dx = Math.abs(btn.dataset.c - selected.dataset.c)
    sum += (dx + dy) * e
    s.value = sum
    selected.style.border = ``
    selected = undefined
  })
}

const input = require(`fs`)
  .readFileSync(0)
  .toString()
  .trim()

const part1 = () => {
  let forward = 0, down = 0, up = 0
  eval(input.replace(/ /g, `+=`))
  console.log(forward * (down - up))
}

part1()

const part2 = () => {
  let forward = 0, aim = 0, depth = 0
  eval(
    input
      .replace(/forward (\d+)/g, `forward += $1; depth += aim * $1;`)
      .replace(/up (\d+)/g, `aim -= $1`)
      .replace(/down (\d+)/g, `aim += $1`)
  )

  console.log(forward * depth)
}

part2()

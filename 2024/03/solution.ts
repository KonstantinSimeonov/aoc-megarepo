const solve = (memory: string) => {
  const instructions = memory
    .match(/mul\(\d+,\d+\)|do\(\)|don't\(\)/g)
    ?.map(
      x => x
        .replace(`don't()`, `enabled = false`)
        .replace(`do()`, `enabled = true`)
    ).join(`;`) ?? `throw "LOL"`

  let [a1, a2, enabled] = [0, 0, true]
  const mul = (x: number, y: number) => {
    a1 += x * y
    enabled && (a2 += x * y)
  }
  eval(instructions)

  return [a1, a2]
}

for (const file of [`input0`, `input1`, `input`]) {
  const input = require(`fs`).readFileSync(`./${file}`, `utf-8`) as string
  console.log(solve(input))
}

const input = require(`fs`).readFileSync(`./input`, `utf8`) 

const input0 = `[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]`

const lines = input.split(`\n`)


const crunch = (() => {
  const closing = {
    '[': ']',
    '(': ')',
    '<': '>',
    '{': '}'
  }

  const points = {
    ')': 3,
    ']': 57,
    '}': 1197,
    '>': 25137
  }

  const last = xs => xs[xs.length - 1]

  const crunch_rec = (line, i, brackets) => {
    if (i >= line.length) return {
      corrupted_score: 0,
      incomplete_score: brackets.reduceRight(
        (score, b) => score * 5 + ` )]}>`.indexOf(b),
        0
      )
    }

      if (`[(<{`.includes(line[i]))
        return crunch_rec(line, i + 1, [...brackets, closing[line[i]]])
      else if (last(brackets) !== line[i])
        return { corrupted_score: points[line[i]], incomplete_score: 0 }

      return crunch_rec(line, i + 1, brackets.slice(0, -1))
    }

  return line => crunch_rec(line, 0, [])
})()

const crunched = lines.map(crunch)

const part1 = crunched.reduce((score, x) => score + x.corrupted_score, 0)

console.log({ part1 })

const part2 = crunched
  .map(x => x.incomplete_score)
  .filter(x => x > 0)
  .sort((a, b) => a - b)

console.log({ part2: part2[part2.length >> 1] })

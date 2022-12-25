declare const require: (name: `fs`) => {
    readFileSync(x: string, y: `utf8`): string
}

const input = require(`fs`)
  .readFileSync(`./input`, `utf8`)
  .trim()

const stuff =
  `let ${input}`
    .replace(/([a-z]{4}) (.) ([a-z]{4})/g, `$1() $2 $3()`)
    .replace(/\n/g, `;\nlet `)
    .replace(/:/g, ` = () =>`)

const stuff2 = `${stuff};\nconsole.log(root());`

eval(stuff2)


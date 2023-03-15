declare const require: (name: `fs`) => {
    readFileSync(x: string, y: `utf8`): string
}

const input = require(`fs`)
  .readFileSync(`./input`, `utf8`)
  .trim()

const stuff =
  `let ${input}`
    .replace(/root: ([a-z]{4}) (.) ([a-z]{4})/, `root = () => [$1(), $3()];`)
    .replace(/([a-z]{4}) (.) ([a-z]{4})/g, `$1() $2 $3()`)
    .replace(/\n/g, `;\nlet `)
    .replace(/:/g, ` = () =>`)

const stuff2 = `${stuff};\nhumn = () => { throw new Error('kekw') };\nconsole.log(root());`
//console.log(stuff2)

let x: string = ``
try {
  eval(stuff2)
} catch (err) {
  x = err.stack.split(`eval code@`)[0].trim().split(`@\n`)
}

console.log(x)

let inp = require(`fs`).readFileSync(`./input0`, `utf-8`);

console.log(
  inp.split(`\n`).reduce(
    (s, line) => {
      const [xs, xe, as, ae] = line.split(/[-,]/).map(Number);
      s[xs] = s[xs] || [];
      s[xs].push(xe);
      s[as] = s[as] || [];
      s[as].push(ae);
      return s
    },
    []
  )
);

const xs =
  inp.trim().split(`\n`).flatMap(
    (line) => {
      const [xs, xe, as, ae] = line.split(/[-,]/).map(Number);
      return [[xs, xe], [as, ae]]
    },
  ).sort((a, b) => (a[0] - b[0]) || (a[1] - b[1]))


let ss = Array.from({ length: xs[xs.length - 1][1] }, () => 0)

for (let i = 0; i < xs.length; ++i) {
  for (let s = xs[i][0]; s < xs[i][1]; ++s) {
    ++ss[s]
  }
}

console.log(ss)

declare const require: (x: string) => any;

const input = require(`fs`).readFileSync(`./input`, `utf-8`) as string;

const solve = (digits: string, step = digits.length / 2) =>
  [...digits]
    .filter((x, i) => x === digits[(i + step) % digits.length])
    .map(Number)
    .reduce((a, b) => a + b, 0);

console.log(
  solve(`1122`, 1),
  solve(`1111`, 1),
  solve(`1234`, 1),
  solve(`91212129`, 1),
  solve(input.trim(), 1),
  solve(`1212`),
  solve(`1221`),
  solve(`123425`),
  solve(`123123`),
  solve(`12131415`),
  solve(input.trim()),
);

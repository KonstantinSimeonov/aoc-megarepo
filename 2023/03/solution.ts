#!/usr/bin/env -S npx tsx

import * as fs from "fs";

const parse = (input: string) => {
  if (input.includes(`,`)) {
    throw new Error(
      `I feel like this isn't really going to work out between us`,
    );
  }

  let id = 0;
  const values: Partial<Record<string, number>> = {};
  const parsed = input
    .trim()
    .replace(/(\d+)/g, (num) => {
      ++id;
      values[id] = Number(num);
      const ids = Array.from(num, () => id).join(`,`)
      return `,${ids},`;
    })
    .replace(/\./g, () => `,0,`)
    .replace(/,,/g, `,`)
    .replace(/^,/gm, ``)
    .replace(/,$/gm, ``)
    .split(`\n`)
    .map((line) => line.split(`,`));

  return { values, parsed };
};

const neighbs = (mat: readonly string[][], r: number, c: number) =>
  new Set(
    [
      [-1, -1],
      [-1, 0],
      [-1, 1],
      [0, -1],
      [0, 1],
      [1, -1],
      [1, 0],
      [1, 1],
    ]
      .map(([dr, dc]) => mat[r + dr]?.[c + dc])
      .filter((x) => x && x !== `0`),
  );

const solve = (path: string) => {
  const input = fs.readFileSync(path, `utf8`);
  const { values, parsed } = parse(input);

  let part1 = 0;
  let part2 = 0;
  for (let i = 0; i < parsed.length; ++i) {
    for (let j = 0; j < parsed[i].length; ++j) {
      const gear = parsed[i][j];
      if (/\d/.test(gear)) continue;

      const ids = neighbs(parsed, i, j);
      const sum = Array.from(ids, (id) => values[id] ?? 0).reduce(
        (a, b) => a + b,
        0,
      );

      part1 += sum;

      if (gear !== `*`) {
        continue;
      }

      const [id_a, id_b] = ids;
      const { [id_a]: a = 0, [id_b]: b = 0 } = values;
      part2 += a * b;
    }
  }

  return { part1, part2 };
};

console.log(solve(`./input`));

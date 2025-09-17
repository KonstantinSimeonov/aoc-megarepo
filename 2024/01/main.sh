#!/usr/bin/env bash
set -eo pipefail

input_file=input

mapfile -t xs < <(cut -d ' ' -f 1 <"$input_file" | sort --numeric-sort)
mapfile -t ys < <(cut -d ' ' -f 4 <"$input_file" | sort --numeric-sort)

declare -i len="${#xs[@]}"
declare -i total=0
for ((i = 0; i < len; i += 1)); do
	diff=$((xs[i] - ys[i]))
	abs=$((diff < 0 ? -diff : diff))
	total=$((total + abs))
done

echo "Part 1: $total"

declare -A ys_freqs
for y in "${ys[@]}"; do
	ys_freqs[$y]=$(("${ys_freqs[$y]:-0}" + 1))
done

declare -i part2=0
for x in "${xs[@]}"; do
	part2=$((part2 + x * ys_freqs[$x]))
done

echo "Part 2: $part2"

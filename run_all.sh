#!/usr/bin/env bash
for d in */; do
    echo $d
    cd $d
    rustc -C opt-level=3 -o solution ./solution.rs
    #time ./solution
    cd ..
done

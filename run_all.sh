#!/usr/bin/env bash
for d in */; do
    echo $d
    cd $d
    for f in *.rs; do
        rustc -C opt-level=3 -o solution "$f"
        time ./solution
    done

    echo DONE
    echo ""

    cd ..
done

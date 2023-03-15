FROM rust

COPY . /aoc
WORKDIR /aoc
CMD ./run_all.sh

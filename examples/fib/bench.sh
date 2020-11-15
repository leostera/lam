#!/bin/bash

N=$1

echo "Running fib(${N})..."

caramelc compile fib.ml
erlc -S fib.erl
erlc fib.erl

cargo run -- build fib.beam -o fib -t native -e fib
cargo run -- build fib.beam -o fib.wasm -t wasm -e fib
cargo run -- build fib.beam -o web/fib.wasm -t web -e fib
cargo run --release -- build fib.beam -o fib.opt -t native -e fib
cargo run --release -- build fib.beam -o fib.opt.wasm -t wasm -e fib
cargo run --release -- build fib.beam -o web/fib.opt.wasm -t web -e fib

hyperfine \
  --warmup 10 \
  "escript fib.erl ${N}" \
  "wasmtime ./fib.opt.wasm ${N}" \
  "wasmtime ./fib.wasm ${N}" \
  "./fib ${N}" \
  "./fib.opt ${N}"

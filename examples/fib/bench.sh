#!/bin/bash

N=$1

echo "Running fib(${N})..."

erlc -S fib.erl
erlc fib.erl

cargo run --release -- build fib.beam -o fib.opt.exe -t native -e fib
cargo run --release -- build fib.beam -o fib.opt.wasm -t wasm -e fib

hyperfine \
  --warmup $2 \
  --ignore-failure \
  "escript fib.erl ${N}" \
  "wasmtime ./fib.opt.wasm ${N}" \
  "./fib.opt.exe ${N}"

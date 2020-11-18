#!/bin/bash

N=$1

echo "Running grep(${N})..."

caramelc compile *.ml
erlc -S *.erl
erlc *.erl

cargo run --release -- build grep.beam -o grep.opt.exe -t native -e grep
cargo run --release -- build grep.beam -o grep.opt.wasm -t wasm -e grep

hyperfine \
  --warmup 50 \
  "escript grep.erl ${N}" \
  "wasmtime ./grep.opt.wasm ${N}" \
  "./grep.opt.exe ${N}"

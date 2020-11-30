#!/bin/bash

erlc -S empty.erl
erlc empty.erl

cargo run --release -- build empty.beam -o empty.opt.exe -t native -e empty
cargo run --release -- build empty.beam -o empty.opt.wasm -t wasm -e empty

hyperfine \
  --warmup 10 \
  --ignore-failure \
  "escript empty.erl ${N}" \
  "wasmtime ./empty.opt.wasm ${N}" \
  "./empty.opt.exe ${N}"

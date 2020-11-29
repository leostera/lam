#!/bin/bash

erlc -S *.erl
erlc *.erl

make install -C ../../

lam build *.beam -o grep.opt.exe -t native -e grep
lam build *.beam -o grep.opt.wasm -t wasm -e grep

bench() {
  local N=$*
  echo "Running grep ${N}..."

  hyperfine \
    --warmup 10 \
    --ignore-failure \
    "grep ${N}" \
    "escript grep.erl ${N}" \
    "wasmtime --dir=. ./grep.opt.wasm ${N}" \
    "./grep.opt.exe ${N}"
}

bench joe *.txt

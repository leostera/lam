#!/bin/bash

hyperfine \
  --warmup 10 \
  "wasmtime ./fib.wasm" \
  "./fib"

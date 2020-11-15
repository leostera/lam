#!/bin/bash

caramelc compile fib.ml
erlc -S fib.erl
erlc fib.erl
cargo run -- build fib.beam -o fib -t native -e fib
# cargo run -- build fib.beam -o fib.wasm -t wasm -e fib

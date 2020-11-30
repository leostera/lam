#!/bin/bash

erlc -S fib.erl
erlc fib.erl
cargo run -- build fib.beam -o fib.exe -t native -e fib
cargo run -- build fib.beam -o fib.wasm -t wasm -e fib

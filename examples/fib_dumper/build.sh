#!/bin/bash -e

erlc -S fib.erl
erlc fib.erl

make build -C ../../
cargo run --release -- compile *.beam -o .
cargo run --release -- link fib.lam -o web/fib.wasm -t web -e fib

cp web/*.wasm web/dist/

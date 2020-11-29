#!/bin/bash -e

erlc -S fib.erl
erlc fib.erl

make build -C ../../
RUST_LOG=trace cargo run -- compile *.beam -o .
cargo run -- link fib.lam -o web/fib.wasm -t web -e fib

cp web/*.wasm web/dist/

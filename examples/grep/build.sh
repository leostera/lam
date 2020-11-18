#!/bin/bash

caramelc compile info.ml grep.ml
erlc -S grep.erl
erlc grep.erl
cargo run -- build grep.beam -o grep.exe -t native -e grep
cargo run -- build grep.beam -o grep.wasm -t wasm -e grep

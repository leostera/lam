#!/bin/bash -e

erlc -S *.erl
erlc *.erl

cargo run -- compile *.beam -o .
cargo run -- link *.lam -o grep.exe -t native -e grep

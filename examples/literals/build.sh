#!/bin/bash -e

erlc -S *.erl
erlc *.erl

cargo run -- compile *.beam -o .
cargo run -- link *.lam -o literals.exe -t native -e literals

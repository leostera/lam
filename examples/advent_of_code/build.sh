#!/bin/bash

ENTRYPOINT=$1

erlc -S *.erl
erlc *.erl
RUST_LOG=trace cargo run -- build *.beam -o advent_of_code.exe -t native -e $ENTRYPOINT 

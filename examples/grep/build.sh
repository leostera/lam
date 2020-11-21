#!/bin/bash

erlc -S *.erl
erlc *.erl
cargo run -- build *.beam -o grep.exe -t native -e grep

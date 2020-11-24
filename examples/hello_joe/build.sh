#!/bin/bash

erlc -S hello_joe.erl
erlc hello_joe.erl

lam build hello_joe.beam -o hello_joe.exe -t native
lam build hello_joe.beam -o hello_joe.wasm -t wasm

mkdir -p web
lam build hello_joe.beam -o web/hello_joe.wasm -t web


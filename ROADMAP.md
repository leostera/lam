# Roadmap

This is a list of all the research and work I'd like to do within the context
of LAM.

#### Milestones

* 2020 Q4 - Milestone 1: BEAM Binaries :hammer:

## 2020 Q4 - Milestone 1: BEAM Binaries :hammer:

The initial goal here is to package a bunch of .beam files into a single
relocatable binary for a specific architecture (eg, only
`x86_64-unknown-linux-gnu`), that can run:

* Natively
* On a WASI enabled runtime
* On a browser through WebAssembly

There should be a small cli, that we can run:

```sh
$ lam build *.beam -o bin.exe -t native
$ ./bin.exe
Hello, Joe!

$ lam build *.beam -o bin.wasm -t wasm
$ wasmtime bin.wasm
Hello, Joe!

$ lam build *.beam -o js.wasm -t web
$ parcel serve index.html
...

# and in a broswer going to the url we see in the JS console
> Hello, Joe!
```

## Enough Bytecode support to build small CLI tools

## Enough NIFs to build small CLI tools


## Future Work :crystall_ball:

- [ ] Processes + GC
- [ ] Message passing
- [ ] FFI
- [ ] Multi-Core Scheduling 

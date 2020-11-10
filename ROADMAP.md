# Roadmap

This is a list of all the research and work I'd like to do within the context
of LAM.

#### Milestones

* 2020 Q4 - Milestone 1: Native Binaries :heavy_check_mark:
* 2020 Q4 - Milestone 2: WASM Binaries :heavy_check_mark:
* 2020 Q4 - Milestone 3: ... :hammer:

## 2020 Q4 - Milestone 1: Native Binaries :heavy_check_mark:

The initial goal here is to package a bunch of .beam files into a single
relocatable binary for a specific architecture (eg, only
`x86_64-unknown-linux-gnu`).

There should be a small cli, that we can run:

```sh
$ lam build *.beam -o bin.exe -t native
$ ./bin.exe
Hello, Joe!
```

Things that we should have:

* Io module should work

## 2020 Q1 - Milestone 2: WASM Binaries :heavy_check_mark:

In short, the same things we had working on milestone 1 run on a browser. In
particular we should be able to run:

```sh
$ lam build *.beam -o bin.wasm -t wasm
$ wasmtime bin.wasm
Hello, Joe!
```

## Enough Bytecode support to build small CLI tools

## Enough NIFs to build small CLI tools

## Web-compatible WASM Binaries

Which means we should be able to load this from a Javascript file, and provide
the necessary FFIs:

```js
load("bin.wasm")
.then( LAM => {
    const LAM = LAM.new({
      io: { format: (...args) => console.log(args) },
    });
    LAM.start();
  });
```

We should also provide this `.js` file as a template with a `start(Args)`
function that you can pass in that will end up as the arguments for the
`main/2` function.


## Future Work :crystall_ball:

- [ ] Processes + GC
- [ ] Message passing
- [ ] FFI
- [ ] Multi-Core Scheduling 

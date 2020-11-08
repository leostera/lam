# Roadmap

This is a list of all the research and work I'd like to do within the context
of LAM.

#### Milestones

* 2020 Q4 - Milestone 1: A BEAM Binary :hammer:

## 2020 Q4 - Milestone 1: A BEAM Binary :hammer:

The initial goal here is to package a bunch of .beam files into a single
relocatable binary for a specific architecture (eg, only
`x86_64-unknown-linux-gnu`).

There should be a small cli, that we can run:

```sh
$ lam build *.beam -o bin.exe
$ ./bin.exe
Hello, Joe!
```

And I expect it to run some basic stuff. Things that we won't have:

* SMP
* Processes
* Message Passing
* NIF / FFIs

Things that we should have:

* Io module should work
* File module should work

## M2: A BEAM .wasm

In short, the same things we had working on milestone 1 run on a browser. In
particular we should be able to run:

```sh
$ lam build *beam -o bin.wasm --target wasm
$ wasmrun bin.wasm
Hello, Joe!
```

Which means we should be able to load this from a Javascript file, and provide
the necessary FFIs:

```js
load("bin.wasm")
.then( mod => {
    const rt = mod.Runtime.new({
      io: { format: (...args) => console.log(args) },
    });
    rt.start();
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

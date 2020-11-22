# Contributing to LAM :building_construction: 

First of all, thanks for taking the time to contribute! :heart::tada::+1:

This project is built with Cargo, but there's a little Makefile on top that
orchestrates parts of the build that should at some point be moved to
`build.rs` scripts.

The code itself lives in the `lib` folder, where the structure is:

```
lib
├── lam-beam
├── lam-bin
├── lam-compiler
├── lam-emu
├── lam-rts-native
├── lam-rts-wasm
└── lam-rts-web
```

Each one of these are different cargo crates that have different dependencies
with each other.

* `lam-beam` &mdash; defines the interface to the BEAM bytecode, how to read it
  and how we operate on it
* `lam-bin` &mdash; is the command line interface
* `lam-compiler` &mdash; has the logic to build the binaries and translate the
  BEAM bytecode to the LAM bytecode for later execution. This crate depends on
  the different runtimes, as they are embedded into it.
* `lam-emu` &mdash; the core emulator system, including the definition of the
  instruction set, literal values, a process, process queues, the scheduler,
  the runtime interface, and a coordinator of schedulers that can be used for
  SMP.
* `lam-rts-native` &mdash; the native runtime system, implementing the system
  calls necessary for LAM to run anywhere that Rust can compile
* `lam-rts-wasm` &mdash; the wasi runtime system, implementing the system calls
  necessary for LAM to run anywhere that WASI can run
* `lam-rts-web` &mdash; the web runtime system, implementing the system calls
  necessary for LAM to run on any WebAssembly enabled browser

To build and install the project locally, it should suffice to run `make build`
and `make install`. 

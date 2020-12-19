<h1>
░█▒░▒▄▀▄░█▄▒▄█ ...:rocket:<br />
▒█▄▄░█▀█░█▒▀▒█ 
</h1>

> A Little Actor Machine that runs on Native and WebAssembly

## What is LAM?

LAM is a research project exploring a **lightweight alternative to the BEAM**
that runs as **Native and WebAssembly binaries**, both on WASI-enabled systems
and browsers.

It will supports:

* Immutable functional programming with proper tail calls
* Concurrency via processes and message passing
* Multi-core scheduling (except on the Web)
* Erlang/OTP architectural patterns (such as supervision trees)

It will _not support_ hot-code reloading or distribution, and so not all BEAM
programs will be supported.

It will be useful for building:

* short-lived, fast-startup services (e.g AWS Lambdas / Google Cloud Functions)
* web apps
* fast command line tools
* native GUI applications

## Great! Can I use it now?

Not yet! There's plenty of work to be done for it to be fully usable, but we
keep a few tracking issues here so its easier for you to see the progress:

* [BEAM Compatibility Status](https://github.com/AbstractMachinesLab/lam/issues/4)
* [LAM Specification Status](https://github.com/AbstractMachinesLab/lam/issues/5)
* [Erlang BIF Support across Runtimes](https://github.com/AbstractMachinesLab/lam/issues/6)

## Getting Started

You can download the latest binary from the [releases
page](https://github.com/AbstractMachinesLab/lam/releases). After
unpacking it you should be able to add it to your PATH env and start playing
around with the `lam` binary.

Like this:

```sh
# in this example I'm running linux with glibc
$ wget https://github.com/AbstractMachinesLab/lam/releases/download/v0.0.5/lam-v0.0.5-x86_64-unknown-linux-gnu.tar.gz
$ tar xzf lam-*
$ export PATH=$(pwd)/lam/bin:$PATH
```

Now we can do a quick test. Make a file `test.erl` with this contents:

```erl
-module(test).

-export([main/1]).

main([]) -> ok;
main([Name|T]) ->
  io:format(<<"Hello, ~p!\n">>, [Name]),
  main(T).
```

And we can compile it to BEAM byte code and use LAM to build a binary for it,
like this:

```sh
$ erlc test.erl
$ lam build test.beam --output test.exe --target native --entrypoint test
$ ./test.exe Joe Robert Mike
Hello, Joe!
Hello, Robert!
Hello, Mike!
```

## How does it work?

LAM compiles your .beam files ahead of time into a representation that's
optimized for running them.

Then it bundles that with the appropriate target runtime into some binary
output.

```
                       binary
                    instructions
                   +------------+              output
 .beam files +---->| 1001101110 |-----+    +-----------+
                   +------------+     |    | .exe      |
                                      |--->| .wasm     |
                   +-------------+    |    | .wasm/.js |
                   | LAM RunTime |----+    +-----------+
                   +-------------+
```

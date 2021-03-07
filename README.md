<div align="center">
  <a href="https://lam.run/" target="_blank">
    <img width="80" src="https://raw.githubusercontent.com/AbstractMachinesLab/lam/main/docs/lam.png" alt="LAM logo">
  </a>
  <p>&nbsp;</p>
</div>

![Main workflow](https://github.com/AbstractMachinesLab/lam/workflows/Main%20workflow/badge.svg)

**LAM** is a lightweight, universal virtual machine for writing scalable and
reliable applications that run natively and on
[WebAssembly](https://webassembly.org).

It is inspired by [Erlang](https://erlang.org) and
[Lua](https://www.lua.org/start.html), and it is compatible with the [Erlang
VM](https://erlang.org).

LAM lets you reuse the same programming paradigm, known for being
**productive**, across your entire application stack.

Come join us on [Discord](https://discord.gg/v5aAqKq6Rs)! (Ps: we share a
server with [Caramel](https://caramel.run))

## Features

* Runs Natively and on WebAssembly -- pick and choose your runtime!
* Easy to Target -- a small and specified bytecode with a text and binary format
* Erlang VM compatibility -- run your existing Erlang, Elixir, Caramel, and Gleam code
* Seamless multi-core -- built to scale from one to thousands of cores for free
* Extreme reliability -- use Erlang's OTP supervision patterns

## Status

Still under heavy development!

There's plenty of work to be done for it to be fully usable, but we keep a few
tracking issues here:

* [LAM Specification Status](https://github.com/AbstractMachinesLab/lam/issues/5)
* [Targetability Status](https://github.com/AbstractMachinesLab/lam/issues/7)
* [WebAssembly and Native Runtime Support](https://github.com/AbstractMachinesLab/lam/issues/8)

The Erlang and Elixir ecosystem compatibility is tracked here:
* [Erlang VM Compatibility Status](https://github.com/AbstractMachinesLab/lam/issues/4)
* [Erlang/Elixir Support Across Runtimes](https://github.com/AbstractMachinesLab/lam/issues/6)

## Getting Started

You can download the latest binary from the [releases
page](https://github.com/AbstractMachinesLab/lam/releases). After
unpacking it you should be able to add it to your PATH env and start playing
around with the `lam` binary.

Like this:

```sh
# in this example I'm running linux with glibc
$ wget https://github.com/AbstractMachinesLab/lam/releases/download/v0.0.7/lam-v0.0.7-x86_64-unknown-linux-gnu.tar.gz
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

# Roadmap

This is a list of all the research and work I'd like to do within the context
of LAM.

#### Milestones

* 2020 Q4 - Milestone 1: BEAM Binaries :heavy_check_mark:
* 2020 Q4 - Milestone 2: Single Process Programs :hammer:
* Milestone 3: Multi-Process Programs :crystal_ball:
* Milestone 4: Multi-core Processing :crystal_ball:
* Milestone 5: Native Extensions :crystal_ball:
* Milestone 6: WebIDL API :rocket:
* Milestone 7: Native Graphics :rocket:

## 2020 Q4 - Milestone 1: BEAM Binaries :heavy_check_mark:

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

## 2020 Q4 - Milestone 2: Single Process Programs :hammer:

For this milestone, I'd like to be able to write arbitrary single-process
Erlang programs. So we should support a larger surface area of the BEAM
bytecode including:

- [ ] y register and heap allocations
- [ ] all value creation
- [ ] all calls ops
- [ ] all tests

The goal here would be to be able to create any sort of values that are valid
in Erlang, and call functions in any way valid in Erlang as well, so that 
we can build some command line tools.

Some things that should be possible are:

* run infinitely: `f() -> f().` should just run and run and run.
* run relatively fast

## Milestone 3: Multi-Process Programs :crystal_ball:

Implement the rest of the bytecode for message passing including:

* spawn, kill, monitor, link
* send, receive, select_receive, remove_message, wait

At this point I'd expect us to support the core OTP modules: `supervisor` and
`gen_server`.

## Milestone 4: Multi-core Processing :crystal_ball:

Implement symmetric multi-processing to spread the work across several cores.

## Milestone 5: Native Extensions :crystal_ball:

To support building useful native and web applications, we need to figure out
how to allow people to link custom code that interfaces with the runtime, and
expose it in a transparent way.

For example, natively, we may want to open a socket to make an HTTP request. On
a browser, we may want DOM manipulation. On other runtimes (like wasi), we may
want other things to be available.

But regardless of the runtime, the calls should look completely transparent.
Calling `json:parse(String).` should be the exact same for a consumer. Whether
the implementation is fulfilled by bytecode running on LAM, or by native
platform code, should be irrelevant.

## Milestone 6: WebIDL API :rocket:

To make it more useful to build web things, we should generate the bytecode
necessary to interface with the web platform from the WebIDL description. This
is how the `web-sys` crate currently does it.

## Milestone 7: Native Graphics :rocket:

From the work I've done before on the `erlang-gui` project, it should be
possible for us to generate native extensions to Vulkan and Skia to build
native applications.

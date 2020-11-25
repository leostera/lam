# Roadmap

This is a list of all the research and work I'd like to do within the context
of LAM sort of organized in small chunks of work that are more or less self
contained.

There is no promise that I'll address these sequentially or completely, as I'm
mainly guiding the work by building utilities like `cat`, or `grep`.

#### Milestones

* 2020 Q4 - Milestone 1: BEAM Binaries :heavy_check_mark:
* Milestone 2: Single Process Programs :hammer:
* Milestone 3: Concurrency-Oriented Programming :hammer:
* Milestone 4: Multi-Core Scheduling :hammer:
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

## Milestone 2: Fast Single-process Programs :hammer:

For this milestone, I'd like to be able to write arbitrary single-process
Erlang programs. 

The goal here would be to be able to create any sort of values that are valid
in Erlang, and call functions in any way valid in Erlang as well, so that 
we can build some command line tools.

Some things that should be possible are:

* run forever:
  * `f() -> f().` should just run and run and run :heavy_check_mark:

* run fast:
  * no-op boot time should be under 10ms :heavy_check_mark:

* use common data types:
  * atoms :heavy_check_mark:
  * integers :heavy_check_mark:
  * floats
  * binary strings :heavy_check_mark:
  * lists :heavy_check_mark:
  * tuples
  * maps

* first class functions:
  * creating lambdas and applying them :heavy_check_mark:
  * passing lambdas around as values :heavy_check_mark:

## Milestone 3: Concurrency-Oriented Programming :hammer:

Here the goal should be to enable building solutions that scale by
orchestrating hundreds of thousands of processes that communicate with each
other.

We'll need to support:
* spawning processes :heavy_check_mark:
* terminating processes
* monitor / demonitor processes
* link / unlink processes
* sending messages
* selectively receive messages

Some things that should be possible are:
* spawn millions of processes in constant time (shouldn't get slower to spin up
  new processes)
* core OTP modules like `supervisor` or `gen_server` should have all the
  instructions needed to run (give or take small changes)

## Milestone 4: Multi-Core Scheduling :crystal_ball:

The goal here is to
Implement symmetric multi-processing to spread the work across several cores by
means of either job-stealing or some other configurable policy.

Stuff that needs doing:

* maintain and run as many schedulers as cores there are :heavy_check_mark:
* load-balancing / work stealing
* message routing

We should be able to run things like:

* map-reduce patterns, like a `word count` program, across all cores without
  modifications

## Milestone 5: Native Extensions :hammer:

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

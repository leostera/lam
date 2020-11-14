<h1>
░█▒░▒▄▀▄░█▄▒▄█ ...:rocket:<br />
▒█▄▄░█▀█░█▒▀▒█ 
</h1>

> Native and WASM binaries for the BEAM

## What is LAM?

LAM is a **lightweight alternative to the BEAM** that runs as **Native and
WebAssembly binaries**, both on WASI-enabled systems and browsers.

It supports

* Concurrency via processes and message passing
* Multi-core scheduling (except on the Web)

It does not support _hot-code reloading_ and _distribution_ so not all BEAM
programs will be supported.

You should use this if you want to build:

* short-lived, fast-startup services (e.g AWS Lambdas / Google Cloud Functions)
* web apps
* fast command line tools
* native GUI applications

## Getting Started

...

## How does it work?

LAM compiles your .beam files ahed of time into a representation that's
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

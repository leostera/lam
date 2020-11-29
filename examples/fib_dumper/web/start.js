import run_wasm from "./fib.js";

import run_js from "./fib_in_js.js";

run_js()
.then(_ => run_wasm());

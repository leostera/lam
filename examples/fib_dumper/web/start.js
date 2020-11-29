import run_worker from "./fib_wrk.js";

import load_lam from "./fib.js";

import run_js from "./fib_in_js.js";

document.onclick = () => {
  run_js(1000)
  .then(_ => run_worker(1000))
  .then(_ => load_lam().then(lam => {
    const run = () => {
      lam.step();
      requestAnimationFrame(run);
    }
    run();
  }));
}

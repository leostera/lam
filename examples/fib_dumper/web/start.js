import load_lam from "./fib.js";

import run_js from "./fib_in_js.js";

document.onclick = () => {
  run_js().then((_) => load_lam().then(lam => {
    const run = () => {
      lam.step();
      requestAnimationFrame(run);
    }
    run();
  }));
}

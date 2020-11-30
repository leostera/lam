self.addEventListener("message", msg => {
  const n = msg.data;

  let i;
  let fib = []; // Initialize array!

  fib[0] = BigInt(0);
  fib[1] = BigInt(1);
  postMessage({ finished: false, n: fib[0] })
  postMessage({ finished: false, n: fib[1] })
  for (i = 2; i <= n; i++) {
    fib[i] = fib[i - 2] + fib[i - 1];
    postMessage({ finished: false, n: fib[i] })
  }

  postMessage({ finished: true, n: fib[i - 1] })
});

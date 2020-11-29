const run = (n) => {
  const root = document.getElementById("js");

  const t0 = Date.now();
  const startedAt = document.createElement("div");
  startedAt.innerText = Date.now();
  root.appendChild(startedAt)

  const brokeAt = document.createElement("div");
  brokeAt.innerText = "";
  root.appendChild(startedAt)

  const node = document.createElement("div");
  node.innerText = "unstarted"
  root.appendChild(node)

  const fib = (n, b, a, done) => {
    node.innerText = a.toLocaleString('fullwide', {useGrouping: false});
    if (n == 0) return done(a);
    setTimeout(() => {
      fib(n - 1, a + b, b, done)
    });
  };

  const fib_n = (n, then) => fib(n, 1, 0, then);

  return new Promise( (res, _) => {
    fib_n(n, (n) => {
      res(n);
      const finishedAt = document.createElement("div");
      const t1 = Date.now() - t0;
      finishedAt.innerText = t1;
      root.appendChild(finishedAt);
    })
  })
};

export default run;

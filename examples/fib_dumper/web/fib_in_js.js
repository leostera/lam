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
  new Promise( (res, _) => {
    node.innerText = a.toLocaleString('fullwide', {useGrouping: false});
    if (n == 0) return done(a);
    res(() => fib(n - 1, a + b, b, done));
  }).then(k => setTimeout(k, 0))
};

const fib_n = (n) => {
  return new Promise( (done, _) => {
    fib(n, 1, 0, done)
  })
}

const run = () => {
  return fib_n(1000)
  .then((n) => {
    const finishedAt = document.createElement("div");
    const t1 = Date.now() - t0;
    finishedAt.innerText = t1;
    root.appendChild(finishedAt);
  })
};

export default run;

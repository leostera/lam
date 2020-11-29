const run = (n) => {
  const worker = new Worker("fib.worker.js");

  const root = document.getElementById("js-wrk");

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

  return new Promise( (res, _) => {
    worker.onmessage = msg => {
      node.innerText = msg.data.n.toLocaleString('fullwide', {useGrouping: false});
      if (msg.data.finished) {
        const finishedAt = document.createElement("div");
        const t1 = Date.now() - t0;
        finishedAt.innerText = t1;
        root.appendChild(finishedAt);
        return res(msg.data.n)
      }
    }

    worker.postMessage(n)
  })
};

export default run;

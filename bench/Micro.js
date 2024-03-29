import { performance } from "perf_hooks";

export function measure(name) {
  return (action) => () => {
    // warmup
    for (let i = 0; i < 1000; i++) {
      action();
    }
    const start = performance.now();
    let count = 0;
    while (performance.now() - start < 1000) {
      for (let i = 0; i < 100; i++) {
        action();
        count++;
      }
    }
    const total = performance.now() - start;
    const duration = total / count;
    //  console.log(name + ': ' + count + ' in ' + total.toFixed(3) + 'ms');
    console.log(rpad(40, name) + ": " + duration.toFixed(6) + "ms/op");
  };
}

function rpad(n, s) {
  while (s.length < n) {
    s += " ";
  }
  return s;
}

export function getFilters() {
  if (typeof location !== "undefined") {
    // Browser
    const hash = location.hash.substring(1);
    return hash ? [decodeURIComponent(hash)] : ["NONE"];
  } else {
    // Node.js (assumed)
    return process.argv.slice(2);
  }
}

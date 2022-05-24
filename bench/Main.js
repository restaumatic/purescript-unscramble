import {performance} from 'perf_hooks';

export function generateData() {
  const data = range(100).map(productId => ({
    name: 'product' + productId,
    description: 'desc' + productId,
    photo: productId % 2 === 0 ? 'photo' : null,
    url: 'product url',
    enabled: productId % 3 !== 0,
    containsAllergens: true,
    sizes: ["Small", "Big", "Large"],
    price: [randomPrice(), randomPrice(), randomPrice()],
    additions: range(10).map(additionId => ({
      name: 'addition' + additionId,
      description: 'desc' + additionId,
      photo: additionId % 3 === 0 ? 'photo' : null,
      price: [randomPrice(), randomPrice(), randomPrice()],
      enabled: true,
      minQuantity: 0,
      maxQuantity: 5,
    })),
  }));
  return data;
}

function range(n) {
  return [...Array(n).keys()];
}

function randomPrice() {
  return Math.floor(Math.random() * 20);
}

export function measure(name) {
  return action => () => {
    const N = 100;
  //  console.log(JSON.stringify(action(), null, 2));
    for(let i = 0; i < N; i++) {
      action();
    }
    const start = performance.now();
    for(let i = 0; i < N; i++) {
      action();
    }
    const duration = (performance.now() - start) / N;
    console.log(name + ': ' + duration.toFixed(3) + 'ms');
  };
}

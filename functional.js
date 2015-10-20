
let map = (fn, array) => array.map(fn)
let times = (x, fn) => [for (i of Array(n).keys()) fn(i)]
let range = (start, end) => [...Array(end - start + 1)].map((_, i) => start + i)

let ideal = map(x => [x, x * 2], range(-10, 10))

let add = (a, b) => a + b
let sub = (a, b) => a - b
let mult = (a, b) => a * b
let mod = (a, b) => a % b
let div = (a, b) => b === 0 ? 0 : a / b

console.log (ideal)

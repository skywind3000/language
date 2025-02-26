
var s = 'test', n = 1, b = true;

var S = new String(s);
var N = new Number(n);
var B = new Boolean(b);

s.hello = 123;
console.log(s.hello);

S.hello = 123;
console.log(S.hello);

console.log(s == S, s === S, s === S.valueOf());

console.log(typeof s);
console.log(typeof S);
console.log(Object.prototype.toString.apply(s));
console.log(Object.prototype.toString.apply(S));



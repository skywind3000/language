
var s = 'test', n = 1, b = true;

var S = new String(s);
var N = new Number(n);
var B = new Boolean(b);

s.hello = 123;
console.log(s.hello);

S.hello = 123;
console.log(S.hello);

console.log(s == S, s === S, s === S.valueOf());



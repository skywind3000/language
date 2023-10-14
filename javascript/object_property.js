var o = {};

Object.defineProperty(o, 'x', { value: 1, 
	writable: true, enumerable: true, configurable: true});

console.log(o.x);

o.x = 10;
console.log(o.x);

console.log(Object.keys(o));

Object.defineProperty(o, 'x', {enumerable: false});
console.log(Object.keys(o));

Object.defineProperty(o, 'x', {writable: false});
o.x = 20;
console.log(o.x);



function inherit(p) {
	if (p == null)
		throw TypeError();
	if (Object.create)
		return Object.create(p);
	var t = typeof(p);
	if (t != 'function' && t != 'object')
		throw TypeError();
	function f() {};
	f.prototype = p;
	return new f();
}

var o = inherit({y:2});

o.x = 1;

console.log(Object.keys(o));

console.log(o.propertyIsEnumerable('x'));
console.log(o.propertyIsEnumerable('y'));
console.log(o.propertyIsEnumerable('toString'));

for (var v in o) {
	console.log(v);
}


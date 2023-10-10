function inherit(p) {
	if (p == null)
		throw TypeError();
	if (Object.create && 0)
		return Object.create(p);
	var t = typeof(p);
	if (t != 'function' && t != 'object')
		throw TypeError();
	function f() {};
	f.prototype = p;
	return new f();
}

var o = {x: 'dont change this value'};
var x = inherit(o);

console.log(x);



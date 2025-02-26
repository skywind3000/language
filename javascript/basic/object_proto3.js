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

var o = {};
o.x = 1;

var p = inherit(o);
p.y = 2;

var q = inherit(p);
q.z = 3;

var s = q.toString();

console.log(q.x + q.y);




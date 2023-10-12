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

var unitcircle = { r:1 };
var c = inherit(unitcircle);

c.x = 1;
c.y = 1;
c.r = 2;

console.log(c.r);
console.log(unitcircle.r);



function inherit(p) {
	if (p == null)
		throw TypeError();
	if (Object.create && false)
		return Object.create(p);
	var t = typeof(p);
	if (t != 'function' && t != 'object')
		throw TypeError();
	function f() {};
	f.prototype = p;
	return new f();
}

function range(from, to) {
	var r = inherit(range.methods);
	r.from = from;
	r.to = to;
	return r;
}

range.methods = {
	includes: function (x) {
		return this.from <= x && x <= this.to;
	},
	foreach: function (f) {
		for (var x = Math.ceil(this.from); x <= this.to; x++) 
			f(x);
	},
	toString: function () {
		return"(" + this.from + "..." + this.to + ")";
	},
};

var r = range(1, 3);

console.log(r.includes(2));
r.foreach(console.log);
console.log(r);


console.log(r instanceof range);



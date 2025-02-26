
function type(o) {
	var t, c, n;
	if (o === null) return "null";
	if (!o == o) return 'nan';
	if ((t = typeof o) !== 'object') return t;
	if ((c = classof(o)) !== 'Object') return c;
	if (o.constructor && typeof o.constructor === 'function'
		&& (n = o.constructor.getName()))
		return n;
	return 'Object';
}

function classof(o) {
	return Object.prototype.toString.call(o).slice(8, -1);
}

Function.prototype.getName = function () {
	if ('name' in this) return this.name;
	this.name = this.toString().match(/function\s*([^(]*)\(/)[1];
	return this.name;
}

function Range(){}

var r = new Range();

console.log(type(r));

var m = new Map();
console.log(type(m));
console.log(typeof Map);


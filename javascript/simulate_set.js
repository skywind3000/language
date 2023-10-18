function objectId(o) {
	var prop = '|**objectid**|'
	if (!o.hasOwnProperty(prop)) {
		var callee = arguments.callee;
		if (!callee.hasOwnProperty('next')) {
			callee.next = 100;
		}
		o[prop] = callee.next++;
		return o[prop];
	}
}

function Set() {
	this.values = {};
	this.n = 0;
	this.add.apply(this, arguments);
}

Set._v2s = function (val) {
	switch (val) {
	case undefined: return 'u';
	case null: return 'n';
	case true: return 't';
	case false: return 'f';
	default:
		switch (typeof val) {
		case 'number': return '#' + val;
		case 'string': return '+' + val;
		default:
			return '@' + objectId(val);
		}
	}
}

Set.prototype.add = function() {
	for (var i = 0; i < arguments.length; i++) {
		var val = arguments[i];
		var str = Set._v2s(val);
		if (!this.values.hasOwnProperty(str)) {
			this.values[str] = val;
			this.n++;
		}
	}
	return this;
}

Set.prototype.remove = function() {
	for (var i = 0; i < arguments.length; i++) {
		var str = Set._v2s(arguments[i]);
		if (this.values.hasOwnProperty(str)) {
			delete this.values[str];
			this.n--;
		}
	}
	return this;
}

Set.prototype.contains = function(value) {
	var str = Set._v2s(value);
	return this.values.hasOwnProperty(str);
}

var s = new Set();

s.add('123');
s.add(1);
s.add(2);

console.log(s.contains(5));
console.log(s.contains(2));
console.log(s.contains('__proto__'));


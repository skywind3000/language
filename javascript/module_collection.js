
var collection;
if (!collection) collection = {};

collection.set = (function namespace() {

	var next = 100;

	function objectId(o) {
		var prop = '|**objectid**|'
		if (!o.hasOwnProperty(prop)) {
			var callee = arguments.callee;
			o[prop] = next++;
		}
		return o[prop];
	}

	function v2s(val) {
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

	function Set() {
		this.values = {};
		this.n = 0;
		this.add.apply(this, arguments);
	}

	Set.prototype.contains = function (value) {
		return this.values.hasOwnProperty(v2s(value));
	};

	Set.prototype.size = function () { return this.n; };

	Set.prototype.add = function () {
		for (var i = 0; i < arguments.length; i++) {
			var val = arguments[i];
			var str = v2s(val);
			if (!this.values.hasOwnProperty(str)) {
				this.values[str] = val;
				this.n++;
			}
		}
		return this;
	};

	Set.prototype.remove = function () {
		for (var i = 0; i < arguments.length; i++) {
			var str = v2s(arguments[i]);
			if (this.values.hasOwnProperty(str)) {
				delete this.values[str];
				this.n--;
			}
		}
		return this;
	};

	// module exports
	return {
		Set: Set,
	};
}());


var s = new collection.set.Set();
var o1 = {};
var o2 = {};
var o3 = {};

s.add('123');
s.add(1);
s.add(2);
s.add(o1, o2);

console.log(s.contains(5));
console.log(s.contains(2));
console.log(s.contains(o1), s.contains(o2), s.contains(o3));




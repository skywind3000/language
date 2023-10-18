function extend(target, o) {
	var names = Object.getOwnPropertyNames(o);
	for (var i = 0; i < names.length; i++) {
		if (names[i] in target) continue;
		var desc = Object.getOwnPropertyDescriptor(o, names[i]);
		Object.defineProperty(target, names[i], desc);
	}
}

function defineClass(constructor, methods, statics) {
	if (methods) extend(constructor.prototype, methods);
	if (statics) extend(constructor, statics);
	return constructor
}

var SimpleRange = defineClass(
	function (f, t) { this.f = f; this.t = t; },
	{
		includes: function(x) { return this.f <= x  && x <= this.t; },
		toString: function() { return this.f+"..."+this.t; },
	},
	{
		upto: function (t) { return new SimpleRange(0, t); },
	},
);




function classof(o) {
	if(o===null)return"Null";
	if(o===undefined)return"Undefined";
	return Object.prototype.toString.call(o).slice(8,-1);
}


function Range(from, to) {
	this.from = from;
	this.to = to;
}

Range.prototype = {
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

class Range2 {

};

var r = new Range(1, 3);

console.log(classof(r));
console.log(Object.prototype.toString.call(r));
console.log(Object.prototype.toString.call(Range));
console.log(Object.prototype.toString.call(Range2));

var s1 = 'hello, world';
var s2 = new String('world');
console.log(s1.length);
console.log(Object.prototype.toString.call(s1));
console.log(Object.prototype.toString.call(s2));
console.log(typeof s1);
console.log(typeof s2);



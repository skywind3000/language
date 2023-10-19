
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



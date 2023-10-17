
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

var r = Object.create(Range.prototype);
Range.call(r, 1, 3);

console.log(r.includes(2));
r.foreach(console.log);
console.log(r);

console.log(typeof r);

console.log(r instanceof Range);



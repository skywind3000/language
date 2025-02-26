
var o = {
	m: function () {
		var self = this;
		console.log(this === o);
		f();
		function f() {
			console.log(this === o);
			console.log(self === o);
		}
	}
};

o.m();



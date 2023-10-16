
function constfuncs() {
	var funcs = [];
	for (var i = 0; i < 10; i++) {
		funcs[i] = function() {return i;}
	}
	return funcs;
}

var funcs = constfuncs();
console.log(funcs[5]());


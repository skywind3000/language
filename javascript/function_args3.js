
function foo(x, y) {
	console.log(x, y);
	arguments[1] = 99;
	console.log(x, y);
}

var x = 10, y = 20;

foo(x, y);

console.log(x, y);



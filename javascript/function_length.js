
function check(args) {
	var actual = args.length;
	var expected = args.callee.length;
	if (actual != expected) {
		throw Error("Expected " + expected + " args, got " + actual);
	}
}

function f(x, y, z) {
	check(arguments);
	return x + y + z;
}

console.log("expecting to pass");

f(1, 2, 3);

console.log("expecting to crash");

f(1);


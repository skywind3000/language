"use strict";

function foo(x, y) {
	console.log(arguments.length);
}

foo(1);
foo(1, 2);
foo(1, 2, 3);
foo(1, 2, 3, 4);



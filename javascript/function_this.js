"use strict";

function test_this() {
	console.log('this', this);
}

test_this();

var o = {x:1, y:2};

o.m = test_this;

o.m();



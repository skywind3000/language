function foo(index) {
	for (var i = 0; i < 10; i++) {
		function bar() {
			console.log('foo', index, 'bar', i);
		}
		bar();
	}
}

foo(10)
foo(20)



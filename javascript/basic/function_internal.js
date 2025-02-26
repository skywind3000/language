function foo(index) {
	for (var i = 0; i < 10; i++) {
		try {
			function bar() {
				console.log('foo', index, 'bar', i);
			}
			bar();
		}
		catch {
		}
	}
}

foo(10)
foo(20)




let scope = 'global';

function foo() {
	console.log('scope', scope);
	let scope = 'local';
	console.log('scope', scope);
}

foo();



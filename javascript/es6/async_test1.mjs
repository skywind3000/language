
async function foo(x) {
	return 100 * x;
}

let y = await foo(99);

console.log(y);

let m = await import('./module_export.mjs');

console.log(m.bar(33, 2));



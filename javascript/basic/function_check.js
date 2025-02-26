
function isFunction(x) {
	return Object.prototype.toString.call(x) === '[object Function]';
}

function f() {}

console.log(isFunction(f));
console.log(Object.prototype.toString.call(f));


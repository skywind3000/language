function classof(o) {
	if (o === null) return "Null";
	if (o === undefined) return 'Undefined';
	return Object.prototype.toString.call(o).slice(8, -1);
}

console.log(classof(classof));
console.log(classof);
console.log(Object.prototype.toString.call(classof));



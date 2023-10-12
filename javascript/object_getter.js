var o = {
	data: 10,
	get access_prop() { return this.data; },
	set access_prop(value) { this.data = value; },
}

console.log(o.access_prop);
o.access_prop = 33;
console.log(o.access_prop);



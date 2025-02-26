
function addPrivateProperty(o, name, predicate) {
	var value;
	o['get' + name] = function() { return value; };
	o['set' + name] = function(v) { 
		if (predicate && !predicate(v)) 
			throw Error('set' + name + ':invalid value ' + v);
		else
			value = v;
	};
}

var o = {}

addPrivateProperty(o, 'Name', function (x) {
	return typeof x == 'string'; });

o.setName('Frank');
console.log(o.getName());

try {
	o.setName(0);
}
catch {
	console.log('error here');
}



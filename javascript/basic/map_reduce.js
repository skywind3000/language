
var map = function(a, f) {
	var results = [];
	for (var i = 0; i < a.length; i++) {
		if (i in a) {
			results[i] = f.call(null, a[i], i, a);
		}
	}
	return results;
}

var reduce = function(a, f, initial) {
	var i = 0, len = a.length, accumulator;
	if (arguments.length > 2) accumulator = initial;
	else {
		if (len == 0) throw TypeError();
	}
	while (i < len) {
		if (i in a) {
			accumulator = a[i];
			i++;
			break;
		}
		i++;
	}
	if (i == len) throw TypeError();
	while (i < len) {
		if (i in a) {
			accumulator = f.call(undefined, accumulator, a[i]);
		}
		i++;
	}
	return accumulator;
}

var data=[1, 1, 3, 5, 5];

var sum = function(x, y) { return x + y; };
var square = function(x) { return x * x; };
var mean = reduce(data, sum) / data.length;
var deviation = map(data, function(x) {return x - mean});

var stddev = Math.sqrt(reduce(map(deviation, square), sum) / (data.length - 1));

console.log('mean', mean);
console.log('diviation', deviation);
console.log('stddev', stddev);



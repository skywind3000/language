
function flexsum() {
	var total = 0;
	for (var i = 0; i < arguments.length; i++) {
		var element = arguments[i];
		var n = 0;
		if (element == null) continue;
		else if (Array.isArray(element)) {
			n = flexsum.apply(this, element);
		}
		else if (typeof element === 'function') {
			n = Number(element());
		}
		else if (isFinite(element)) {
			n = Number(element);
		}
		total += n;
	}
	return total;
}


console.log(flexsum([1, 2, 3], 4, 5, [6, 7]));




var sum = function(x, y) { return x + y; };

var succ = sum.bind(null, 1);

console.log(succ(2));

function f(y, z) { return this.x + y + z; };

var g = f.bind({x:1}, 2);

console.log(g(3));





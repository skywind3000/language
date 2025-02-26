const Person = {
	isHuman: false,
	printIntroduction: function () {
		console.log(`My name is ${this.name}. Am I human? ${this.isHuman}`);
	}
}

Person.printIntroduction();

console.log(Person);

const me = Object.create(Person);

me.name = 'Matthew';
me.isHuman = true;

me.printIntroduction();

console.log(me.__proto__);

console.log(Object);
console.log(Person.printIntroduction);




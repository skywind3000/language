function regularFunction() {
	const promise = asyncFunction();
	console.log(promise); // Promise {<pending>}

	// can't use await here, but can use .then
	promise.then(result => {
		console.log("Async result:", result);
	});

	console.log("This runs before async completes");
}

async function asyncFunction() {
	await new Promise(r => setTimeout(r, 1000));
	return "Done";
}

regularFunction();



function foo() {
	{
		var x = 10;
	}
	try { console.log(x); } catch { console.log('failed to access x'); }
	{
		let y = 20;
	}
	try { console.log(y); } catch { console.log('failed to access y'); }
}

foo();



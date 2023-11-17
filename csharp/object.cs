using System;

class Hello {
	static void Main(string[] args) {
		Console.WriteLine("Hello, World");
		int x = 10;
		Object y = x;
		int z = (int)y;
		Console.WriteLine(y);
		Console.WriteLine(z);
	}
}


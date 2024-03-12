using System;
using System.IO;

class Hello {
	static void Main(string[] args) {
		string text = File.ReadAllText("fileread.cs");
		Console.Write(text);
	}
}


using System;

namespace Asclib {
	class Test {
		public static void test() {
			Console.WriteLine("NameSpace");
		}
	}
}

class Variables {
	static void Main(string[] args) {
		Console.WriteLine(sizeof(byte));
		Console.WriteLine(sizeof(sbyte));
		Console.WriteLine(sizeof(char));
		Console.WriteLine(sizeof(short));
		Console.WriteLine(sizeof(uint));
		Console.WriteLine(sizeof(int));
		Console.WriteLine(sizeof(UInt32));
		Console.WriteLine(sizeof(Int16));
		AscCore.Test.test();
	}
}

// docmake.py
// commandz.py

// @command(build/win32): echo build1
// @command(run):


using System;
using System.Collections;

class Hello {
	static void Main(string[] args) {
		Console.WriteLine("Hello, World !!");
		Hashtable ht = new Hashtable();
		ht.Add(1, 1024);
		ht.Add("key1", "value1");
		ht.Add("key2", 2048);
		Console.WriteLine(ht.Contains(1));
		Console.WriteLine(ht.Contains(5));
		Console.WriteLine(ht.Contains("key1"));
		ht.Remove("key1");
		foreach (DictionaryEntry it in ht) {
			Console.WriteLine(" - {0} and {1}", it.Key, it.Value);
		}
		int i = 0;
		while (true) {
			if (++i > 10) break;
			Console.WriteLine(i);
		}
		string s1 = "  haha  ";
		Console.WriteLine(s1);
		Console.WriteLine(s1.Trim());
		Console.WriteLine(s1);
	}
}




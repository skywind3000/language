using System;

class TestArrayClass {
	static void Main() {
		Console.WriteLine("arrays");
		int[] array1 = new int[5];
		int[] array2 = new int[] { 1, 3, 5, 7, 9 };
		int[] array3 = { 1, 2, 3, 4, 5, 6 };
		int[,] matrix1 = new int[2, 3];
		int[,] matrix2 = { {1, 2, 3}, {4, 5, 6} };
		int[][] jagged = new int[6][];
		jagged[0] = new int[4] {1, 2, 3, 4};
	}
}



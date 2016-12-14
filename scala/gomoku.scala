
class ChessBoard {
	val board = new Array[Array[Int]](15);
	for (i <- 0 until 15) {
		board(i) = new Array[Int](15);
	}

	clear

	def clear {
		board.foreach { row =>
			for (i <- 0 until row.length) row(i) = 0;
		}
	}

	def get(row:Int, col:Int): Int = {
		if (row < 0 || col < 0 || row >= 15 || col >= 15)
			return 0
		return board(row)(col)
	}

	def set(row:Int, col:Int, value:Int) {
		if (row >= 0 && row < 15 && col >= 0 && col < 15) {
			board(row)(col) = value
		}
	}

	def show {
		println("  A B C D E F G H I J K L M N O")
		for (i <- 0 until 15) {
			print(Character.toChars(65 + i)(0))
			print(" ")
			for (j <- 0 until 15) {
				val c = get(i, j);
				if (c == 0) {
					print(". ");
				}	
				else if (c == 1) {
					print("O ");
				}
				else {
					print("X ");
				}
			}
			println
		}
		println
	}
}

val cb = new ChessBoard
cb.set(7, 7, 1);
cb.set(8, 6, 2);
cb.show



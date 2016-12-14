
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
	}

	def walk(row:Int, col:Int, color:Int) : Boolean = {
		List((0, 1), (1, 1), (1, 0), (1, -1)).foreach { item =>
			var count = 0
			var c = col
			var r = row
			for (i <- 0 until 5) {
				if (get(r, c) == color) count += 1
				r += item._1
				c += item._2
			}
			if (count == 5) {
				return true
			}
		}
		return false
	}

	def check(color:Int) : Boolean = {
		for (row <- 0 until 15) {
			for (col <- 0 until 15) {
				if (walk(row, col, color)) {
					return true
				}
			}
		}
		return false
	}
}


class ChessGame
{
	val cb = new ChessBoard
	val name = List("Player A", "Player B")

	def main() {
		var turn = 1
		var over = false
		var run = 1
		while (!over) {
			println("<Round " + run.toString + ">")
			cb.show
			print(name(turn - 1))
			val movement = scala.io.StdIn.readLine(", make your move: ").trim.toUpperCase
			if (movement.length != 2) {
				println("invalid input, enter two characters for row and column")
			}
			else if (movement(0) < 'A' || movement(0) > 'O') {
				println("invalid row position, must between 'A' and 'O'")
			}
			else if (movement(1) < 'A' || movement(1) > 'O') {
				println("invalid col position, must between 'A' and 'O'")
			}
			else {
				val row = movement(0) - 'A'
				val col = movement(1) - 'A'
				if (cb.get(row, col) != 0) {
					println("you can not move there")
				}
				else {
					println(name(turn - 1) + " move to row " + movement(0) 
						+ " and col " + movement(1))
					cb.set(row, col, turn)
					if (cb.check(turn)) {
						over = true;
						println(name(turn - 1) + " wins !!")
					}
					else {
						run += 1
						if (turn == 1) turn = 2
						else turn = 1
					}
				}
			}
			println()
		}
	}
}




if (true) {
	val game = new ChessGame
	game.main()
}	else {
	val board = new ChessBoard
	board.set(7, 7, 1)
	board.set(8, 8, 1)
	board.set(9, 9, 1)
	board.set(10, 10, 1)
	board.set(11, 11, 1)
	board.show
	println("check1: " + board.check(1))
}



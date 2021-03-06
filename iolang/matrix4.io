
Matrix := List clone do (
	dim := method(x, y,
		m := Matrix clone
		z := 0
		fuck := 3
		for (i, 0, y - 1,
			n := list()
			for (j, 0, x - 1, n append(0))
			m append(n))
		m row := y
		m col := x
		m)

	get := method(x, y, (self at(y)) at(x))
	set := method(x, y, z, (self at(y)) atPut(x, z))

	rotate := method(
		row := self row
		col := self col
		m := Matrix dim(self col, self row)
		for (j, 0, row - 1,
			for (i, 0, col - 1, m set(i, j, self get(j, i))))
		m
		)

	display := method(
		row := self row
		col := self col
		writeln("Matrix(", col, ", ", row, "):")
		for (i, 0, row - 1, 
			for (j, 0, col - 1, 
				write(self get(j, i) asString, " "))
			write("\n"))
		writeln()
		)

	save := method(filename,
		f := File with(filename)
		f remove
		f openForUpdating
		f write((self col) asString, "\n")
		f write((self row) asString, "\n")
		for (i, 0, (self row) - 1,
			for (j, 0, (self col) - 1,
				f write(self get(j, i) asString, "\n")))
		f close
		)

	load := method(filename,
		f := File with(filename)
		f openForReading
		col := (f readLine) asNumber
		row := (f readLine) asNumber
		m := Matrix dim(col, row)
		for (i, 0, row - 1, 
			for (j, 0, col - 1,
				m set(j, i, f readLine asNumber)))
		f close
		m)
)



m := Matrix dim(4, 4)
n := Matrix dim(2, 2)


m set(0, 1, 1)
m set(1, 0, 3)
m set(0, 3, 8)
m set(2, 2, 6)
m set(3, 0, 9)
n set(1, 0, 99)
n set(0, 1, 3)

m display
m rotate display
m save("m.txt")

q := Matrix load("m.txt")
q display



Matrix := List clone

Matrix do (
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

	rotate := method(
		row := self row
		col := self col
		m := Matrix dim(self col, self row)
		for (j, 0, row - 1,
			for (i, 0, col - 1, m set(i, j, self get(j, i))))
		m
		)

	save := method(filename,
		f := File with(filename)
		f remove
		f openForUpdating
		row := self row
		col := self col
		for (i, 0, row - 1, 
			for (j, 0, col - 1, 
				f write(self get(j, i) asString, " "))
			f write("\n"))
		f close)
)


Matrix get := method(x, y, (self at(y)) at(x))
Matrix set := method(x, y, z, (self at(y)) atPut(x, z))

m := Matrix dim(4, 4)
n := Matrix dim(2, 2)


m set(0, 1, 1)
m set(1, 0, 3)
m set(0, 3, 8)
m set(2, 2, 6)
m set(3, 0, 9)
n set(1, 0, 99)
n set(0, 1, 3)

m println
n println
m rotate println
n rotate println

m save("m1.txt")
m rotate save("m2.txt")



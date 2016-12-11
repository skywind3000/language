
Matrix := List clone

Matrix dim := method(x, y,
		m := Matrix clone
		z := 0
		for (i, 0, y - 1,
			n := list()
			for (j, 0, x - 1, n append(0))
			m append(n))
		m)

Matrix get := method(x, y, (self at(y)) at(x))
Matrix set := method(x, y, z, (self at(y)) atPut(x, z))

m := Matrix dim(4, 4)
n := Matrix dim(2, 2)


m set(2, 2, 100)
n set(1, 0, 99)

m println
n println



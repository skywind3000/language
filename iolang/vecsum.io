
Number sum := method(self)
List + := method(y, (self sum) + (y sum))

x := list(list(1,2,3), list(4,5,6), list(7,8,9))

sum := method(x, 
		m := 0
		for (i, 0, (x size) - 1, m = m + ((x at(i)) sum))
		m)

sum(x) println




fib := method(x, 
		if (x == 0, 1,
				if (x == 1, 1, 
					(fib(x - 2)) + (fib(x - 1))
				)
		   )
		)


for (i, 0, 20, writeln("[", i, "] ", fib(i)))


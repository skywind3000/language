

curlyBrackets := method(
			r := List clone
			call message arguments foreach(arg,
					x := r doMessage(arg)
					r append(x)
				)
			r
	   )


x := { 1, 2, 3, 4 }
x println


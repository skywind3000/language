((lambda (x y z)
	(begin
		(display x)
		(newline)
		(display y)
		(newline)
		(display z)
		(newline)
	)
) 1 2 3)

((lambda x
	(begin
		(display x)
		(newline)
	)
) 1 2 3 4)

((lambda (x . y)
	(begin
		(display x)
		(newline)
		(display y)
		(newline)
	)
) 1 2 3 4)

(define (t x . y)
	(begin
		(display x)
		(newline)
		(display y)
		(newline)
	)
)

(t 1 2 3 4)


(define display3
	(lambda (arg1 arg2 arg3) 
		(begin
			(display arg1)
			(display " ")
			(display arg2)
			(display " ")
			(display arg3)
			(newline)
		)
	)
)

(display3 7 8 9)
(display3 7 8 9)


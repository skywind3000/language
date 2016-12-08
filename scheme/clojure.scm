(define cc 
	(lambda (x) 
		(lambda () 
			(begin 
				(set! x (+ x 1)) 
				(display x) 
				(newline) 
				x
			)
		)
	)
)

(define c1 (cc 10))
(define c2 (cc 20))
(c1)
(c1)
(c2)
(c2)
(c1)
(c1)


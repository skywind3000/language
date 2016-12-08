(define x 100)
(set! x 10)
(display x)
(newline)
(begin
	(set! x 200)
	(display x)
	(newline)
)
(display x)
(newline)


(define add2 
	(lambda (x y) (begin
		(set! x (+ x y))
		(display "FUCK")
		(newline)
		x
		)))

(define x 100)
(define y 200)
(display (add2 x y))
(newline)
(display x)
(newline)


(define add2 (
		lambda (x) 
		(set! x (+ x 2)) 
		x
	)
)

(display (add2 x))
(newline)
(display x)
(newline)



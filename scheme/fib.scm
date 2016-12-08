(define (fib x y)
	(display x) 
	(newline)
	(or (> y 20) (fib y (+ x y))))

(fib 1 1)


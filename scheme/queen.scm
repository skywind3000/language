(define (iter i n f) (
	if (< i n) (begin (f i) (iter (+ i 1) n f))) )

(define (list-make x n) (
	if (= n 0) '() (append (list x) (list-make x (- n 1)))) )

(define (vector-make x n) (
	list->vector (list-make x n)) )

(define (vector-append x y)
	(list->vector (append (vector->list x) (vector->list y))) )

(define (check-row-x-row-y vec x y) 
	(define n (vector-length vec))
	(define u (vector-ref vec (if (>= x 0) x 0)))
	(define v (vector-ref vec (if (>= y 0) y 0)))
	(define d (abs (- x y)))
	(cond
		((or (= x 0) (= x y)) #t)
		((or (= v (- u d)) (= v (+ u d)) (= u v)) #f)
		(else #t)
	))

(define (check-row vec x)
	(define (check-iter y)
		(if (or (= x 0) (< y 0)) #t 
		(and (check-row-x-row-y vec x y) 
		(check-iter (- y 1)) )))
	(if (= x 0) #t (check-iter (- x 1))) )

(define counter 0)

(define (view vec row b)
	(display row)
	(display ": ")
	(display vec)
	(display (if b " ok" ""))
	(newline))

(define (queen vec row)
	(define s (vector-length vec))
	(iter 0 s (lambda (i) 
		(vector-set! vec row i)
		(if (check-row vec row) 
			(if (= row (- s 1)) 
			(begin
				(set! counter (+ counter 1))
				(display "Find: ")
				(display vec)
				(display " ")
				(display counter)
				(newline)
			)
			(queen vec (+ row 1)))
		)
	))
	)

(define vec (vector-make 0 8))
(queen vec 0)


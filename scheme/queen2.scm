(define (f vec x y) 
	(define u (vector-ref vec (if (>= x 0) x 0)))
	(define v (vector-ref vec (if (>= y 0) y 0)))
	(define d (- x y))
	(or (< y 0) (not (or (= u v) (= v (+ u d)) (= v (- u d)) ))) )

(define (g vec x y)
	(or (or (= x 0) (< y 0)) (and (f vec x y) (g vec x (- y 1)) )))

(define (h vec x)
	(cond ((or (= x 0) (g vec x (- x 1))) 1) (else 0)))

(define (p vec x i)
	(define n (vector-length vec))
	(vector-set! vec x i)
	(cond	((or (>= i n) (>= x n)) 0)
		((= x (- n 1)) (+ (h vec x) (p vec x (+ i 1))))
		((= (h vec x) 1) (+ (p vec (+ x 1) 0) (p vec x (+ i 1))) ) 
		(else (p vec x (+ i 1))) ))

(display (p #(0 1 2 3 4 7 6 7) 0 0))
(newline)



(define (iter f i n) 
	(if (< i n) (begin (f i) (iter f (+ i 1) n) ))	)

(define (row n m)
	(define x (- (/ m 2) n 1))
	(iter (lambda (i) (display " ")) 0 x)
	(iter (lambda (i) (display "#")) 0 (+ n n 1))
	(newline) 
	)

(define max 40)
(define num 10)

(iter (lambda (i) (row i max)) 0 num)
(iter (lambda (i) (row (- num i) max)) 0 (+ num 1))
(newline)



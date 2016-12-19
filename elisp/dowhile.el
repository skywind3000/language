(require 'cl)

(setq x 0)

(loop do
	  (message "%d" x)
	  (setq x (1+ x))
	  while
	  (< x 10))

(setq z (loop with result = '()
	  for i downfrom 10
	  for j from 0 by 2
	  while (< j 10)
	  do
	  (push (+ i j) result)
	  finally
	  return (nreverse result)))

(message "%s" z)

(message "square: %s" (loop for i in '(1 2 3 4 5 6)
							collect (* i i)))



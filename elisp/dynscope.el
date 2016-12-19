(defun foo ()
  (let ((x 6))
	(bar)
	x))

(defun bar ()
  (let ((x 7))
  (setq x 7)))

(message "x is %d" (foo))


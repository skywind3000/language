(defun foo ()
  (let ((x 6))
	(bar)
	x))

(defun bar ()
  (setq x 7))

(message "x is %d" (foo))


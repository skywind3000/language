(defmacro my-cadr (x)
  (list 'car (list 'cdr x)))

(message "%s" (my-cadr '(1 2 3 4)))

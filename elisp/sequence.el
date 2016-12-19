(setq x [1 2 3 4 5])

(message "%d %s %s" (length x) x (elt x 2))

(setq y [10 20 30])

(setq z (vconcat x y))
(message "x is %s" x)
(message "y is %s" y)
(message "z is %s" z)


(setq h (make-hash-table))
(message "%s" h)




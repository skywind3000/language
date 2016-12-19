(setq x 10 total 0)

(while (>= x 0)
  (message (format "%d %d" total x))
  (setq total (+ total x))
  (setq x (- x 1)))


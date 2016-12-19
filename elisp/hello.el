(message "Hello, \"Emacs\" World !!")
(setq my-text (format "hello %s %s %c %d %s" 1 2 ?X #x10 (or t 0)))
(message (format "%s %s" [1 2 3 4 5] 'adsfasdf))
(message (format "%s %s %s" '(1 2 (+ 3 4))
				 (quote (1 2 (+ 3 4)))
				 (list 1 2 (+ 3 4)))
				 )
(message my-text)
(message (format "compare: %s" (/= 32 (+ 1 1))))
(message (concat "this" " is " "concat string"))

(setq foo 100)
(message (format "compare: %s" (eq foo 100)))
(message (format "vector: %s" (vector 1 2 3 4)))

(if (= foo 100)
	(progn (message "true") (message "haha"))
  (message "false") )

(when (eq foo 100)
  (message "foo is 100")
  (message "hahahah"))

(setq foo 200)

(cond
 (100
  (message "foo=100")
  (message "end1"))
 (200
  (message "foo=200")
  (message "end2")))



 
'( (apple . "red"))


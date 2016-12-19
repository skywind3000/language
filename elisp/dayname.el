(defun day-name (day)
  (catch 'return
	(case day
	  (0
	   (throw 'return "Sunday"))
	  (6
	   (throw 'return "Saturday"))
	  (t
	   (throw 'return "weekday")))))

(message "%s %s" (day-name 0) (day-name 5))

(message "size %d %d %d"
		 (length (list 1 2))
		 (length (vector 1 2 3 4))
		 (length "Hello"))


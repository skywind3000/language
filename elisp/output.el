(defun my-append-string-to-file (s filename)
  (with-temp-buffer
	(erase-buffer)
    (insert s)
    (write-region (point-min) (point-max) filename nil)))

(my-append-string-to-file "fuck you\n" "output.txt")

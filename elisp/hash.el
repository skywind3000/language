(setq table (make-hash-table :test 'equal))

(setf (gethash "one" table) 100)
(message "%s" (gethash "one" table))
(message "%s" (gethash "two" table))

(puthash "three" 3 table)

(defun print-hash (key value)
  (message "key=%s value=%s" key value))

(print-hash 10 20)
(maphash 'print-hash table)
(message "hash code %d" (sxhash "HELLO"))




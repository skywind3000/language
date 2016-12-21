(message "buffer-file-name: %s" (buffer-file-name))
(message "file-name-directory: %s" (file-name-directory (buffer-file-name)))
(message "file-name-nondirectory: %s" (file-name-nondirectory  (buffer-file-name)))
(message "file-name-sans-extension: %s"
		 (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
(message "file-name-extension: %s" (or (file-name-extension (buffer-file-name)) ""))


(message "%s" (expand-file-name "abc.cpp" "d:/acm/github"))
(message default-directory)

(message "pwd: %s" (pwd))


(message "replace: %s" (replace-regexp-in-string "%F" "HAHA.el" "gcc %F  %F %f xadf" t))

(message "%s" (regexp-quote "%F\\ s/ldfkdj'^$"))

(message "line: %s" (1+ (count-lines 1 (point))))

(message "point: %s %s/%s" (point) (point-min) (point-max))

(message "current-column: %s" (current-column))

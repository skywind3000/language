(message "buffer-file-name: %s" (buffer-file-name))
(message "file-name-directory: %s" (file-name-directory (buffer-file-name)))
(message "file-name-nondirectory: %s" (file-name-nondirectory  (buffer-file-name)))
(message "file-name-sans-extension: %s"
		 (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
(message "file-name-extension: %s" (or (file-name-extension (buffer-file-name)) ""))


(message "%s" (expand-file-name "abc.cpp" "d:/acm/github"))
(message default-directory)

(message "pwd: %s" (pwd))

(let ((case-fold-search nil))
  (message "rep: %s" (replace-regexp-in-string "%F" "HAHA" "gcc %F %f %F" t t)))

(message "replace: %s" (replace-regexp-in-string "%F" "^$\\" "gcc %F  %f %f xadf"  nil t))

(message "%s" (regexp-quote "%F\\ s/ldfkdj'^$+."))

(message "line: %s" (1+ (count-lines 1 (point))))

(message "point: %s %s/%s" (point) (point-min) (point-max))

(message "current-column: %s" (current-column))


(message "%s" (shell-quote-argument "ad dd \"sdfdsf\""))
(let ((default-directory "E:\\"))
  (message default-directory)
 ; (compile "cd")
  )
(message default-directory)

(compile "cd")

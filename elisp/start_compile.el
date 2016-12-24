(setq option '((:cwd . "e:\\lab\\casuald") (:open . :same)))
;(vimmake-run "python e:/lab/timer.py " :process option)
;(vimmake-run "cmd" :windows option)
(vimmake-run "dir" :process option)

(message "default-directory: %s" default-directory)

(setq info (vimmake-buffer-info))
(vimmake-init-environ info)

(message "%d/%d"
		 (cdr (assoc :line info))
		 (cdr (assoc :column info))
		 )
;

(defun display-line-number ()
	(let ((info (vimmake-buffer-info))
		  (x 0) (y 0) (z 0) (w ""))
	  (setq x (cdr (assoc :column info)))
	  (setq y (cdr (assoc :line info)))
	  (setq z (point))
	  (setq w (cdr (assoc :cword info)))
	  (setq s (cdr (assoc :csymbol info)))
	  (message "%d: %d/%d -> <%s> (%s)" z y x w s))
	)

(display-line-number)
(global-set-key [f11] (lambda() (interactive) (display-line-number)))


(message "endup")




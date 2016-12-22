(setq option '((:cwd . "e:\\lab\\casuald")))
(vimmake-run "python e:/lab/timer.py " :process option)
;(vimmake-run "cmd" :windows option)

(message "default-directory: %s" default-directory)





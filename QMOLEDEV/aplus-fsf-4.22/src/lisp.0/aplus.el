;; Copyright (c) 2001 Morgan Stanley Dean Witter and Co. All rights reserved.
;; See .../src/LICENSE for terms of distribution.

;; Loads the Aplus specific settings for xemacs 
;;
(setq load-path 
      (append 
       '("/usr/local/aplus-fsf-4.20/lisp.0") load-path 
       )	
      )
;;
;; Load Aplus and set defaults
;;
(load "xa" nil t)
(setq a-prog "/usr/local/bin/a+")
(setq a-plus t)
;;(setq a-plus-without-s t)
;;(setq a-plus-rest '("u" "arg0" "arg1"))
;;(setq a-mbytes 4)
;;(setq a-mbytes-threshold 256)
;;
;;
;; Load User's custom Aplus Settings
;;
(cond ((file-readable-p "~/.custom/a-options.el")
       (load-file "~/.custom/a-options.el")
       )
      )




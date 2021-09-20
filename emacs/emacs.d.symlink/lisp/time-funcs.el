;;; time-funcs.el --- miscellaneous time utilities.
;;; Copyright 2010(c) Stuart Hacking <stuhacking@gmail.com>.

;; Author: Stuart Hacking <stuhacking@gmail.com>
;; Created: 03-May-2010
;; Version: 1.0

;;; Code:
(require 'time-stamp)

(defun insert-date ()
     (interactive)
     (insert (time-stamp-string "%02d-%3b-%:y")))

(provide 'time-funcs)

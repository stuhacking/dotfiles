;;; init-win.el --- Initialize Windows Specific Customisations.
;;;


;; Consistent Alt-F4 behaviour.
;; Close current frame. If it's the last frame
;; then prompt to ``save-all-buffers-and-kill-emacs''
(defun dismiss-frame-or-exit ()
  "Dismiss the current frame or exit emacs if it is the last frame."
  (interactive)
  (if (<= (length (frame-list)) 1)
      (save-buffers-kill-emacs)
    (delete-frame)))
(global-set-key [M-f4] 'dismiss-frame-or-exit)

;;; init-win.el ends here

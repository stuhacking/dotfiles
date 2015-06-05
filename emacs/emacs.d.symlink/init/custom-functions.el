;; Show XPath in modeline when in nxml mode
(defun nxml-where ()
  "Display the hierarchy of XML elements ending at the element
under the point as a path."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element) ; always returns nil
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
            (format "/%s" (mapconcat 'identity path "/")))))))

;; Command: Describe Function at Point.
(defun describe-function-at-point ()
  "Describe the function under the point."
  (interactive)
  (let ((f (function-called-at-point)))
    (if f
        (describe-function f)
        (message "No function under cursor"))))

;; Borrowed path hack from: http://stackoverflow.com/a/8609349
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to
match that used by the user's shell.

This is particularly useful under Mac OSX if not starting
Emacs.app from a shell."
  (interactive)
  (let ((path-from-shell
         (replace-regexp-in-string
          "[ \t\n]*$" ""
          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

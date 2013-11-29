;;; init.el --- Emacs Initialization

;; Author:   Stuart Hacking <stuhacking@gmail.com>
;; Created:  2006

;; Updated for Emacs 24; probably won't work without modification on
;; earlier emacs versions.

;;; Code:
(require 'cl)
(require 'package)
(package-initialize)

(setq emacs-dir (case system-type
                  ('windows-nt "/emacs-23.3/")
                  (t "/usr/local/share/emacs"))
      my-emacsd "~/.emacs.d/")

(setq custom-file (concat my-emacsd "custom.el"))
(load custom-file)

;; Include Additional Load Paths
(let ((paths (list (concat my-emacsd "lisp/")
                   (concat my-emacsd "lisp/private/")
                   (concat my-emacsd "site-lisp/")
                   (concat my-emacsd "site-lisp/slime/"))))
  (mapc #'(lambda (p) (add-to-list 'load-path p)) paths))

;; Load libraries
(load-library "window-state")           ; Zoom and restore windows in a frame.
(global-set-key [f12] 'toggle-zoom-window)

(load-library "wordcount")              ; Simple word count program.

;;; Load additional initialisations:
(let ((init-dir (concat my-emacsd "init/"))
      ;; Each init file should take the form of (filename [(system-type ...)]
      ;; Where the first element is the filename
      ;;   and the second element is an optional list of systems on which to
      ;;   load the file.
      (init-files (list
                   '("init-kbd")
                   '("init-cl")
                   '("init-org")
                   '("init-win" (windows-nt)))))
  (mapc #'(lambda (f) (load (concat init-dir (first f))))
        (remove-if-not #'(lambda (x) (or (null (second x))
                                         (member system-type (second x))))
                       init-files)))

;;; Auto Modes
(add-to-list 'auto-mode-alist '("\\.php" . php-mode))

;;; Various and sundry functions

;; Command: Describe Function at Point.
(defun describe-function-at-point ()
  "Describe the function under the cursor"
  (interactive)
  (let ((f (function-called-at-point)))
    (if f
        (describe-function f)
      (message "No function under cursor"))))
;; Bind to help key.
(global-set-key [C-f1] #'describe-function-at-point)

;; Borrowed path hack from: http://stackoverflow.com/a/8609349
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not
started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;; always use short y/n confirmations.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable options normally disabled.
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; init.el ends here

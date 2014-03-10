;;; init.el --- Emacs Initialization

;; Author:   Stuart Hacking <stuhacking@gmail.com>
;; Created:  2006

;; Updated for Emacs 24; probably won't work without modification on
;; earlier emacs versions.

;;; Code:
(require 'cl)

;; Enable package manager on emacs
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")))

;; Use separate customisation file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Include Additional Load Paths
(dolist (path (list (concat user-emacs-directory "init/")
                    (concat user-emacs-directory "lisp/")
                    (concat user-emacs-directory "lisp/private/")
                    (concat user-emacs-directory "site-lisp/")))
  (add-to-list 'load-path path))

;; Load libraries
(load-library "wordcount")
(load-library "out-of-time")
(load-library "window-state")           ; Zoom and restore windows in a frame.
(global-set-key [f12] 'toggle-zoom-window)

;;; Load additional init files:
(dolist (init-file '(("init-kbd")
                     ("init-deb")
                     ("init-org")
                     ("init-lisp")
                     ("init-haskell")
                     ("init-py")
                     ("init-win" (windows-nt))))
  (when (or (null (second init-file))
            (member system-type (second init-file)))
    (message "Loading init... " init-file)
    (load (first init-file))))

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
;; Bind to alternate help key.
(global-set-key [M-f1] #'describe-function-at-point)

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

(set-exec-path-from-shell-PATH)

;; always use short y/n confirmations.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable options normally disabled.
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; init.el ends here

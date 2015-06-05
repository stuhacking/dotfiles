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

;; Tidy non-shared files under .emacs.d
(defvar user-emacs-tmp-dir (concat user-emacs-directory "tmp/"))

;; Use separate customisation file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Additional Load Paths
(add-to-list 'load-path (concat user-emacs-directory "init/"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/private"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp/"))

(add-to-list 'load-path (concat user-emacs-directory "site-lisp/magit/"))
(load "magit")

;; Load libraries
(load "wordcount")
(load "out-of-time")
(load "window-state")  ; Zoom and restore windows in a frame.

;; Load Settings
(load "custom-functions")
(load "init-deb")
(load "init-org")
;; (load "init-lisp")
(load "init-haskell")
(load "init-py")

(when (equalp system-type 'windows-nt)
  (load "init-win"))

;; Load keyboard shortcuts
(load "init-kbd")

(setq cookie-file (concat user-emacs-directory "yow.lines"))

;;; Auto Modes
(add-to-list 'auto-mode-alist '("\\.php" . php-mode))

(set-exec-path-from-shell-PATH)

;; always use short y/n confirmations.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable options normally disabled.
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; init.el ends here

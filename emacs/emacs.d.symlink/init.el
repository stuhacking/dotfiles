;;; init.el --- Emacs Initialization

;; Author:   Stuart Hacking <stuhacking@gmail.com>
;; Created:  2006
;; Modified: 12-May-2011

;;; Code:
(require 'cl)

;; Setup System directories
(setq allow-offensive-content-p nil)
(setq emacs-dir (case system-type
                  ('windows-nt "/emacs-23.3/")
                  (t "/usr/local/share/emacs"))
      my-emacsd "~/.emacs.d/")
      
(setq custom-file (concat my-emacsd "custom.el"))
(load custom-file)

;; Include Additional Load Paths
(let ((paths (list (concat my-emacsd "lisp/")
		   (concat my-emacsd "lisp/danger/")
           (concat my-emacsd "site-lisp/")
		   (concat my-emacsd "site-lisp/slime/"))))
  (mapc #'(lambda (p) (add-to-list 'load-path p)) paths))

;; Load libraries
(load-library "out-of-time")
(load-library "groovy-mode")
(load-library "window-state")           ; Zoom and restore windows in a frame.
(load-library "wordcount")              ; Simple wordcount program.
(when allow-offensive-content-p
  (load-library "non-sequitur"))        ; Flame! Program

;; Load additional initialisations:
(let ((init-dir (concat my-emacsd "init/"))
      ;; Each init file should take the form of (filename [(system-type ...)]
      ;; Where the first element is the filename
      ;;   and the second element is an optional list of systems on which to
      ;;   load the file.
      (init-files (list
                   '("init-kbd")
                   '("init-cl")
                   '("init-win" (windows-nt)))))
  (mapc #'(lambda (f) (load (concat init-dir (first f))))
        (remove-if-not #'(lambda (x) (or (null (second x))
                                         (member system-type (second x))))
                       init-files)))

(setq find-function-C-source-directory (concat emacs-dir "src/"))

;;; Additional auto-modes
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))

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

(and (= emacs-major-version 23)
     (defun server-ensure-safe-dir (dir)
       "Noop" t))

;; always use short y/n confirmations.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable options normally disabled.
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; init.el ends here

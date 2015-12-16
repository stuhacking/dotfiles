;;; init-lisp --- Common Options for Lisp style languages.

;;; Commentary:

;; This package provides customizations related to programming
;; in Lisp dialects.  Assume that ELPA is used and load everything
;; on a hook.

;;; Code:

;; Standard Lisp Options
(dolist (hook '(lisp-mode-hook
                clojure-mode-hook
                emacs-lisp-mode-hook
                slime-repl-mode-hook
                cider-repl-mode-hook))
  (add-hook hook
            (lambda ()
              (eval-when-compile
                (require 'rainbow-delimiters))
              (rainbow-delimiters-mode))))

(dolist (hook '(lisp-mode-hook
                clojure-mode-hook
                emacs-lisp-mode-hook
                slime-repl-mode-hook))
  (add-hook hook
            (lambda ()
              (eval-when-compile
                (require 'paredit))
              (paredit-mode))))

;; Emacs Lisp
(dolist (hook '(emacs-lisp-mode-hook))
  (add-hook hook
            (lambda ()
              (turn-on-eldoc-mode)
              (imenu-add-menubar-index)
              (checkdoc-minor-mode)
              (setq lisp-indent-function 'common-lisp-indent-function))))

;; Common Lisp & Slime
(dolist (hook '(lisp-mode-hook))
  (add-hook hook
            (lambda ()
              (eval-when-compile
                (require 'slime)))))

(if (eq system-type 'darwin)
    (setq inferior-lisp-program "ccl")
    (setq inferior-lisp-program "sbcl"))
(setq slime-compile-file-options
      '(:fasl-directory "~/tmp/slime-fasl/"))
(setq lisp-indent-function 'common-lisp-indent-function)
(eval-after-load "slime"
  '(progn
    (slime-setup '(slime-fancy
                   slime-scratch
                   slime-banner
                   slime-autodoc))))


;; Clojure & NRepl
(dolist (hook '(cider-mode-hook
                cider-repl-mode-hook))
  (add-hook hook 'cider-turn-on-eldoc-mode))

(provide 'init-lisp)
;;; init-lisp.el ends here

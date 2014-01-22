;;; Common Options for Lisp style languages

;; Standard Lisp Options
(dolist (hook '(lisp-mode-hook
                clojure-mode-hook
                emacs-lisp-mode-hook
                slime-repl-mode-hook
                cider-repl-mode-hook))
  (add-hook hook
            (lambda ()
              (eval-when-compile
                (require 'rainbow-delimiters)
                (require 'paredit))
              (rainbow-delimiters-mode)
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
                (require 'slime))
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
                                 slime-autodoc)))))))

;; Clojure & NRepl

;;; init-lisp.el ends here.

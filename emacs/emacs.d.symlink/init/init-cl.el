;;; init-cl.el --- Initialise options for Common^W Lisp programming
;;;
(require 'slime)

(if (eq system-type 'darwin)
    (setq inferior-lisp-program "ccl")
    (setq inferior-lisp-program "/usr/bin/sbcl"))

(setq slime-compile-file-options '(:fasl-directory "~/tmp/slime-fasl/"))

;; hook rainbow mode into any lisp mode
(dolist (hook '(lisp-mode-hook
                slime-repl-mode-hook
                emacs-lisp-mode-hook))
  (add-hook hook 'rainbow-delimiters-mode))

;; Slime Setup
(eval-after-load "slime"
  '(progn
    (slime-setup '(slime-fancy
                   slime-scratch
                   slime-banner
                   slime-autodoc
                   slime-highlight-edits))))


;;; init-cl.el ends here

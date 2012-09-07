;;; init-cl.el --- Initialise options for Common^W Lisp programming
;;;
(require 'rainbow-delimiters)
(require 'cldoc)
(require 'slime)

(setq slime-lisp-implementations
      '((ccl ("ccl"))
        (sbcl ("/usr/local/bin/sbcl") :coding-system utf-8-unix)))

(setq inferior-lisp-program
      (case system-type
        ('winnt "sbcl")
        ('darwin "ccl")))

;;(eval-after-load "rainbow-delimiters"
;;  (setq-default frame-background-mode 'dark))

;; hook rainbow mode into any lisp mode
(dolist (hook '(lisp-mode-hook
                slime-repl-mode-hook
                emacs-lisp-mode-hook))
  (add-hook hook 'rainbow-delimiters-mode))

;; hook cldoc into common lisp modes
(dolist (hook '(lisp-mode-hook
                slime-repl-mode-hook))
  (add-hook hook 'cldoc-mode))

;; Slime Setup
(eval-after-load "slime"
  (progn
    (slime-setup '(slime-fancy slime-banner))))

(when (eq system-type 'darwin)
  (setq common-lisp-hyperspec-root "file:~/dev/lisp/HyperSpec-7-0/HyperSpec/"))


;;; init-cl.el ends here

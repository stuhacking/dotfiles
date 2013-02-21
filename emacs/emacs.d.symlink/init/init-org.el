;;; init-cl.el --- Initialise options for Common^W Lisp programming
;;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/lisp/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/contrib/lisp/" t)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-src-fontify-natively t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;; init-cl.el ends here

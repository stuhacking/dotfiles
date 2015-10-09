;;;; Work Laptop fixes

;(add-to-list 'load-path (concat user-emacs-directory "elpa/pkg-info-0.4/"))
;(add-to-list 'load-path (concat user-emacs-directory "elpa/epl-0.5/"))
;(add-to-list 'load-path (concat user-emacs-directory "elpa/paredit-22/"))
;(add-to-list 'load-path (concat user-emacs-directory "elpa/rainbow-delimiters-1.3.4/"))

(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10.169.*\\)")
        ("http" . "www-proxy.uk.oracle.com:80")
        ("https" . "www-proxy.uk.oracle.com:80")))

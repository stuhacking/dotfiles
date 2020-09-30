
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist '(("\".\"" . "\"~/.emacs.d/bak/\"")))
 '(c-basic-offset 4)
 '(c-default-style
   '((c-mode . "k&r")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu")))
 '(c-doc-comment-style
   '((c-mode . javadoc)
     (c++-mode . javadoc)
     (java-mode . javadoc)))
 '(column-number-indicator-zero-based nil)
 '(column-number-mode t)
 '(custom-file "~/.emacs.d/lisp/settings.el")
 '(custom-safe-themes
   '("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
 '(exec-path
   '("~/bin" "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_14" "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_14" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin" "/Library/TeX/texbin" nil))
 '(fill-column 72)
 '(fringe-mode '(nil . 0) nil (fringe))
 '(global-visual-line-mode t)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(ispell-local-dictionary-alist
   '(("English" "\"[A-Za-z]\"" "\"[^A-Za-z]\"" "\"[']\"" nil
      ("\"-B\"" "\"-d\"" "\"english\"" "\"--dict-dir\"" "\"/Library/Application Support/cocoAspell/aspell6-en-6.0.0\"")
      "~tex" iso-8859-1)))
 '(ispell-program-name "aspell")
 '(mouse-avoidance-mode 'animate nil (avoid))
 '(org-agenda-files '("~/.emacs.d/calendar.org" "~/dev/gamedev/crpg/plan.org"))
 '(org-babel-load-languages
   '((lisp . t)
     (emacs-lisp . t)
     (js . t)
     (C . t)
     (perl . t)
     (shell . t)))
 '(org-confirm-babel-evaluate t)
 '(org-default-notes-file "~/.emacs.d/notes" t)
 '(org-directory "~/.emacs.d/orgfiles/")
 '(org-html-doctype "html5")
 '(org-log-done 'time)
 '(org-pretty-entities nil)
 '(org-src-tab-acts-natively t)
 '(org-startup-folded 'content)
 '(package-selected-packages
   '(delight diminish use-package powerline c-eldoc org-bullets darkroom paredit column-enforce-mode rust-mode rustic cider htmlize clang-format rainbow-mode rainbow-delimiters magit projectile-speedbar projectile glsl-mode solarized-theme))
 '(projectile-tags-backend 'auto)
 '(projectile-tags-command "ctags-exuberant -Re -f \"%s\" %s \"%s\"")
 '(python-shell-interpreter "python3")
 '(python-shell-interpreter-args "-m IPython --simple-prompt -i")
 '(require-final-newline t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(slime-repl-history-remove-duplicates t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(visual-line-fringe-indicators '(left-curly-arrow nil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(when (display-graphic-p)
  (custom-theme-set-faces
   'solarized-dark
   '(show-paren-match ((t (:weight bold :inverse-video t))))
   '(font-lock-doc-face ((t (:foreground nil :inherit font-lock-comment-face))))))

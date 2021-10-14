
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/sav/" t)))
 '(backup-directory-alist '(("\".\"" . "\"~/.emacs.d/bak/\"")))
 '(before-save-hook '(delete-trailing-whitespace))
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
 '(comment-fill-column 76)
 '(custom-file "~/.emacs.d/lisp/settings.el")
 '(custom-safe-themes
   '("2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "37144b437478e4c235824f0e94afa740ee2c7d16952e69ac3c5ed4352209eefb" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
 '(default-input-method "japanese")
 '(exec-path
   '("~/bin" "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_14" "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_14" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin" "/Library/TeX/texbin" nil))
 '(fill-column 80)
 '(fringe-mode '(nil . 0) nil (fringe))
 '(global-hl-line-mode nil)
 '(global-visual-line-mode t)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-frame-alist
   '((vertical-scroll-bars)
     (left-fringe)
     (right-fringe . 0)
     (width . 90)
     (height . 50)))
 '(ispell-local-dictionary-alist
   '(("English" "\"[A-Za-z]\"" "\"[^A-Za-z]\"" "\"[']\"" nil
      ("\"-B\"" "\"-d\"" "\"english\"" "\"--dict-dir\"" "\"/Library/Application Support/cocoAspell/aspell6-en-6.0.0\"")
      "~tex" iso-8859-1)))
 '(ispell-program-name "aspell")
 '(menu-bar-mode nil)
 '(mouse-avoidance-mode 'animate nil (avoid))
 '(org-agenda-files '("~/.emacs.d/calendar.org" "~/dev/gamedev/crpg/plan.org"))
 '(org-babel-clojure-backend 'cider)
 '(org-babel-load-languages
   '((lisp . t)
     (emacs-lisp . t)
     (js . t)
     (C . t)
     (perl . t)
     (shell . t)
     (python . t)
     (clojure . t)))
 '(org-confirm-babel-evaluate t)
 '(org-default-notes-file "~/.emacs.d/notes" t)
 '(org-directory "~/.emacs.d/orgfiles/")
 '(org-html-doctype "html5")
 '(org-log-done 'time)
 '(org-pretty-entities nil)
 '(org-roam-directory "/home/smh/.emacs.d/org-roam/")
 '(org-src-tab-acts-natively t)
 '(org-startup-folded 'content)
 '(package-selected-packages
   '(yaml-mode git-gutter markdown-mode go-mode doom-themes org-roam racket-mode auctex delight diminish use-package powerline c-eldoc org-bullets darkroom paredit column-enforce-mode rust-mode rustic cider htmlize clang-format rainbow-mode rainbow-delimiters magit projectile-speedbar projectile glsl-mode solarized-theme))
 '(projectile-tags-backend 'auto)
 '(projectile-tags-command "ctags-exuberant -Re -f \"%s\" %s \"%s\"")
 '(python-shell-interpreter "python2")
 '(python-shell-interpreter-args "-i")
 '(require-final-newline t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(slime-repl-history-remove-duplicates t)
 '(solarized-high-contrast-mode-line t)
 '(solarized-scale-org-headlines nil)
 '(solarized-scale-outline-headlines nil)
 '(solarized-use-less-bold t)
 '(solarized-use-variable-pitch nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(visual-line-fringe-indicators '(left-curly-arrow nil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-mismatch ((t (:background "#dc322f" :foreground "#FFFBF0" :box (:line-width 2 :color "#FFFBF0" :style released-button) :weight ultra-bold)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-mode-hook (quote (turn-on-font-lock)))
 '(auto-save-list-file-prefix "~/.emacs.d/asave/.saves-")
 '(background-color "#fdf6e3")
 '(background-mode light)
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backup/"))))
 '(c-default-style (quote ((c-mode . "Stroustrup") (c++-mode . "Stroustrup") (java-mode . "java") (awk-mode . "AWK"))))
 '(c-require-final-newline (quote ((c-mode . t) (c++-mode . t) (objc-mode . t) (java-mode . t))))
 '(column-number-mode t)
 '(cursor-color "#657b83")
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-file "~/.emacs.d/custom.el")
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(debug-on-error t)
 '(diary-file "~/.emacs.d/diary")
 '(display-time-day-and-date nil)
 '(display-time-default-load-average nil)
 '(display-time-mail-face nil)
 '(display-time-world-list (quote (("DST5EDT" "Boston") ("DST8PDT" "San Francisco") ("GMT0BST" "Belfast") ("JST-9" "Tokyo"))))
 '(emacs-lisp-mode-hook (quote (turn-on-eldoc-mode imenu-add-menubar-index checkdoc-minor-mode)))
 '(eshell-directory-name "~/.emacs.d/.eshell/")
 '(even-window-heights nil)
 '(find-function-source-path (cons "/emacs-23.1/src/" load-path))
 '(focus-follows-mouse t)
 '(foreground-color "#657b83")
 '(fortune-always-compile nil)
 '(fortune-dir "~/.emacs.d/")
 '(fortune-file "~/.emacs.d/fortunes")
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(global-hl-line-mode nil)
 '(help-mode-hook (quote (variable-pitch-mode)))
 '(hippie-expand-try-functions-list (quote (try-complete-lisp-symbol try-complete-lisp-symbol-partially try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-expand-line try-complete-file-name-partially try-complete-file-name)))
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote right))
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote emacs-lisp-mode))
 '(initial-scratch-message "")
 '(lisp-indent-function (quote common-lisp-indent-function))
 '(mailclient-place-body-on-clipboard-flag nil)
 '(minibuffer-auto-raise t)
 '(mode-require-final-newline (quote visit-save))
 '(mouse-avoidance-mode (quote animate) nil (avoid))
 '(nroff-mode-hook (quote (variable-pitch-mode)))
 '(ns-tool-bar-display-mode nil t)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (C . t) (perl . t) (python . t) (haskell . t) (ruby . t) (js . t) (java . t) (sql . t) (gnuplot . t) (awk . t))))
 '(org-src-tab-acts-natively t)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(recentf-max-saved-items 30)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/.recentf")
 '(require-final-newline (quote visit-save))
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/.emacs-places")
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(size-indication-mode t)
 '(slime-autodoc-use-multiline-p t)
 '(slime-complete-symbol-function (quote slime-fuzzy-complete-symbol))
 '(slime-repl-history-file "~/.emacs.d/slime-history.eld")
 '(snake-initial-velocity-x 1 t)
 '(snake-randomize-apples nil)
 '(speedbar-show-unknown-files t)
 '(tab-width 4)
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(toe-highscore-file "~/.emacs.d/toe-scores")
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(user-full-name "Stuart Hacking")
 '(visible-bell t)
 '(visual-line-mode nil t)
 '(yow-file "~/.emacs.d/yow.lines"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-string-face ((t (:foreground "#2aa198" :weight bold))))
 '(org-block-background ((t (:background "#FFFFFF"))))
 '(org-block-end-line ((t (:inherit org-block-begin-line))) t))


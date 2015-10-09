(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-mode-hook (quote (turn-on-font-lock)))
 '(auto-save-list-file-prefix (concat user-emacs-tmp-dir "asave/saves-"))
 '(backup-directory-alist (list (cons "." (concat user-emacs-tmp-dir "backup/"))))
 '(c-default-style
   (quote
    ((c-mode . "Stroustrup")
     (c++-mode . "Stroustrup")
     (java-mode . "Java")
     (awk-mode . "AWK"))))
 '(c-offsets-alist (quote ((innamespace . -))))
 '(c-require-final-newline
   (quote
    ((c-mode . t)
     (c++-mode . t)
     (objc-mode . t)
     (java-mode . t))))
 '(column-number-mode t)
 '(custom-file (concat user-emacs-directory "custom.el"))
 '(debug-on-error t)
 '(diary-file (concat user-emacs-directory "diary"))
 '(display-time-day-and-date nil)
 '(display-time-default-load-average nil)
 '(display-time-mail-face nil)
 '(display-time-world-list
   (quote
    (("DST5EDT" "Boston")
     ("DST8PDT" "San Francisco")
     ("GMT0BST" "Belfast")
     ("JST-9" "Tokyo"))))
 '(eshell-directory-name (concat user-emacs-tmp-dir "eshell/"))
 '(even-window-heights nil)
 '(fci-rule-color "#073642")
 '(focus-follows-mouse t)
 '(foreground-color "#657b83")
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(global-hl-line-mode nil)
 '(help-mode-hook (quote (variable-pitch-mode)))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hippie-expand-try-functions-list
   (quote
    (try-complete-lisp-symbol try-complete-lisp-symbol-partially try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-expand-line try-complete-file-name-partially try-complete-file-name)))
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote right))
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote emacs-lisp-mode))
 '(initial-scratch-message "")
 '(mailclient-place-body-on-clipboard-flag nil)
 '(minibuffer-auto-raise t)
 '(mode-require-final-newline (quote visit-save))
 '(mouse-avoidance-mode (quote animate) nil (avoid))
 '(nroff-mode-hook (quote (variable-pitch-mode)))
 '(ns-tool-bar-display-mode nil t)
 '(org-agenda-files (concat user-emacs-directory "org-agenda-files"))
 '(org-babel-load-languages (quote ((emacs-lisp . t))))
 '(org-src-tab-acts-natively t)
 '(recentf-max-saved-items 30)
 '(recentf-mode t)
 '(recentf-save-file (concat user-emacs-tmp-dir "recentf"))
 '(require-final-newline (quote visit-save))
 '(safe-local-variable-values (quote ((Eval visual-line-mode t) (Eval visual-line-mode))))
 '(save-place t nil (saveplace))
 '(save-place-file (concat user-emacs-tmp-dir "emacs-places"))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(size-indication-mode t)
 '(speedbar-show-unknown-files t)
 '(tab-width 4)
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(toe-highscore-file (concat user-emacs-tmp-dir "toe-scores"))
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(url-cookie-file (concat user-emacs-tmp-dir "url/cookies/"))
 '(user-full-name "Stuart Hacking")
 '(vc-annotate-background "#93a1a1")
 '(vc-annotate-color-map
   (quote
    ((20 . "#990A1B")
     (40 . "#FF6E64")
     (60 . "#cb4b16")
     (80 . "#7B6000")
     (100 . "#b58900")
     (120 . "#DEB542")
     (140 . "#546E00")
     (160 . "#859900")
     (180 . "#B4C342")
     (200 . "#3F4D91")
     (220 . "#6c71c4")
     (240 . "#9EA0E5")
     (260 . "#2aa198")
     (280 . "#69CABF")
     (300 . "#00629D")
     (320 . "#268bd2")
     (340 . "#69B7F0")
     (360 . "#d33682"))))
 '(vc-annotate-very-old-color "#93115C")
 '(visible-bell t)
 '(visual-line-fringe-indicators (quote (nil nil)))
 '(visual-line-mode nil t)
 '(yow-file (concat user-emacs-directory "yow.lines")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#F4FFF3" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(font-lock-comment-face ((t (:foreground "DarkGoldenRod" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "darkblue"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-function-name-face ((t nil)))
 '(font-lock-keyword-face ((t (:foreground "blue3"))))
 '(font-lock-preprocessor-face ((t (:foreground "Pink3" :slant italic))))
 '(font-lock-string-face ((t (:foreground "grey40" :weight bold))))
 '(font-lock-type-face ((t (:foreground "#C7C600"))))
 '(font-lock-variable-name-face ((t (:foreground "grey20"))))
 '(org-block-begin-line ((t (:inherit org-meta-line :background "grey80" :foreground "orangered" :box (:line-width 2 :color "grey75" :style released-button)))) t)
 '(org-block-end-line ((t (:inherit org-block-begin-line))) t)
 '(rainbow-delimiters-depth-1-face ((t (:foreground "grey70"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "SteelBlue"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "plum"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "DarkGreen"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "plum"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "violet"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "darkred" :foreground "white")))))

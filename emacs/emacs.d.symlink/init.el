;;;; Emacs Init File                                        -*- emacs-lisp -*-
(defconst emacs-start-time (current-time))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;;; Startup:
(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup t
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)


;;; Functions:
(eval-and-compile
  (defun user-path (path)
    (expand-file-name path "~"))

  (defun emacs-path (path)
    (expand-file-name path user-emacs-directory)))


;;; Environment:
(eval-and-compile
  (setq load-path
        (append (delete-dups load-path)
                '("~/.emacs.d/lisp/")))

  (require 'use-package)

  ;; Customizations file.
  (load (emacs-path "lisp/settings")))


;;; Global IDO Search/Open.
(setq ido-create-new-buffer 'always)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Theme Package:
(use-package doom-themes
  :config
  (setq active-theme 'doom-city-lights
        doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (load-theme active-theme t)

  (custom-theme-set-faces
   active-theme
   '(show-paren-match ((t (:weight bold :inverse-video t))))
   '(font-lock-doc-face ((t (:foreground nil
                             :inherit font-lock-comment-face)))))

  ;; This seems to work better than modifying the default face when using
  ;; themes.
  (set-face-attribute 'default nil
                      :family "Iosevka"
                      :height 110))

;; Programming modes standard preferences:
(use-package magit
  :bind (("<f12>" . magit-status)))

(use-package darkroom
  :bind (("<f7>" . darkroom-tentative-mode)))

(use-package diminish
  :ensure t
  :config
  (diminish 'abbrev-mode)
  (diminish 'eldoc-mode)
  (diminish 'column-enforce-mode))

;; Enable spell checking
(use-package flyspell
  :config
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (diminish 'flyspell-mode "Ⓢ")
  (diminish 'flyspell-prog-mode "ⓢ"))

(use-package git-gutter
  :init (global-git-gutter-mode +1))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (diminish 'rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode)
  (diminish 'rainbow-mode))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (diminish 'paredit-mode "☯"))

(use-package clojure-mode
  :ensure t
  :config
  (define-clojure-indent
    (returning 1)
    (testing-dynamic 1)
    (testing-print 1))
  (add-hook 'clojure-mode-hook #'paredit-mode))

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t))

(use-package yaml-mode
  :ensure t)

(add-hook 'prog-mode-hook
	  (lambda ()
            (projectile-mode +1)
            (column-enforce-n 100)))

(add-hook 'projectile-mode-hook
	  (lambda ()
	    (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)))


;; Alternate binding for completion (I like tab behaviour to be 'always-indent).
(global-set-key (kbd "<C-tab>") 'complete)
(global-set-key (kbd "M-#") 'goto-line)

(global-set-key (kbd "C-/") 'isearch-forward)
(global-set-key (kbd "C-?") 'isearch-forward-regexp)
(global-set-key (kbd "C-*") 'highlight-symbol-at-point)

;;; Org-mode Customization:
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-default-notes-file "~/.emacs.d/notes.org")
(setq org-capture-templates
      '(("n" "Note" entry (file+headline "~/.emacs.d/notes.org" "Notes")
         "* %U %?\n\n %i\n %a")
        ("v" "Vacation" entry (file+headline "~/.emacs.d/calendar.org" "Vacation")
         "** %? (,)\n   %t--%t")))


;;; SLIME Customization:
(use-package slime
  :commands slime
  :init
  (load (expand-file-name "~/dev/lisp/ql/slime-helper.el"))
  (setq inferior-lisp-program "sbcl"
        slime-compile-file-options '(:fasl-directory "~/dev/tmp/slime-fasl/")
        slime-contribs '(slime-fancy
                         slime-scratch
                         slime-banner
                         slime-autodoc))

  :preface
  (defun slime-description-fontify ()
    "Fontify sections of SLIME description."
    (with-current-buffer "*slime-description*"
      (highlight-regexp
       (concat "^Function:\\|"
               "^Macro-function:\\|"
               "^Its associated name.+?) is\\|"
               "^The .+'s arguments are:\\|"
               "^Documentation:$\\|"
               "^Its.+\\(is\\|are\\):\\|"
               "^On.+it was compiled from:$")
       'hi-green-b)))

  (defadvice slime-show-description (after slime-description-fontify activate)
    "Fontify sections of SLIME Description."
    (slime-description-fontify)))

;;;; init.el ends here.

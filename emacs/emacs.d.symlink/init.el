;;;; Emacs Init File
(defconst emacs-start-time (current-time))

;; Added by Package.el.  This must come before configurations of installed
;; packages.  Don't delete this line.
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

;; Hack for proper focus behaviour on closing a client.
(defadvice server-switch-buffer (around test activate)
  (when (ad-get-arg 0)
    ad-do-it))

;; Functions:
(eval-and-compile
  (defun user-path (path)
    (expand-file-name path "~"))

  (defun emacs-path (path)
    (expand-file-name path user-emacs-directory))

  (defun find-init ()
    (interactive)
    (find-file (emacs-path "init.el"))))


;; Environment:
(eval-and-compile
  (setq load-path
        (append (delete-dups load-path)
                '("~/.emacs.d/lisp/")))

  (require 'use-package)

  ;; Customizations file.
  (load (emacs-path "lisp/settings")))


;; Global IDO Search/Open.
(setq ido-create-new-buffer 'always)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Appearance:
(use-package doom-themes
  :config
  (setq active-theme 'doom-one
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


;;; Org-mode Customization:

(use-package org
  :ensure nil
  :config
  (setq org-directory (emacs-path "org/")
        org-html-doctype "html5"
        org-log-done 'time
        org-pretty-entities nil
        org-src-tab-acts-natively t
        org-startup-folded 'content)

  ;; Org-Babel
  (setq org-babel-clojure-backend 'cider
        org-confirm-babel-evaluate t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp . t)
     (emacs-lisp . t)
     (js . t)
     (C . t)
     (perl . t)
     (shell . t)
     (python . t)
     (clojure . t))))

(use-package org-agenda
  :ensure nil
  :after org
  :bind ("C-c a" . org-agenda))

(use-package org-capture
  :ensure nil
  :after org
  :bind ("C-c c" . org-capture)
  :config
  (setq org-default-notes-file (emacs-path "org/notes.org")
        org-default-tasks-file (emacs-path "org/tasks.org"))
  ;; Capture templates
  (setq org-capture-templates
        `(("n" "Note"
           entry
           (file+headline ,org-default-notes-file "Notes")
           ;; template
           "* %U %?\n\n %i\n %a"))))


;; Programming modes standard preferences:
(use-package magit
  :ensure t
  :bind (("M-g s" . magit-status)))

(use-package darkroom
  :bind (("<f12>" . darkroom-tentative-mode)))

(use-package diminish
  :ensure t
  :config
  (diminish 'abbrev-mode)
  (diminish 'eldoc-mode)
  (diminish 'column-enforce-mode))

;; Enable spell checking
(use-package flyspell
  :hook ((text-mode-hook . flyspell-mode)
         (prog-mode-hook . flyspell-prog-mode))
  :config
  (diminish 'flyspell-mode "Ⓢ")
  (diminish 'flyspell-prog-mode "ⓢ"))

(use-package git-gutter
  :init (global-git-gutter-mode +1)
  :config
  (diminish 'git-gutter-mode "△"))

(use-package ws-butler
  :ensure t
  :hook (prog-mode . ws-butler-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (diminish 'rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :hook prog-mode
  :config
  (diminish 'rainbow-mode))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (ielm-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (eval-expression-minibuffer-setup . paredit-mode)
         (clojure-mode . paredit-mode))
  :config
  (diminish 'paredit-mode "♎"))

(use-package clojure-mode
  :ensure t
  :config
  (define-clojure-indent
    (returning 1)
    (testing-dynamic 1)
    (testing-print 1)))

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

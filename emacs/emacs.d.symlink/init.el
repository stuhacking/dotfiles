
;;;; Emacs Init File                                        -*- emacs-lisp -*-
(defconst emacs-start-time (current-time))


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.


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
    (expand-file-name path user-emacs-directory))

  (defun office-laptop-p ()
  "Return t if the current machine is a work laptop."
  (string-equal "shacking-mac" (system-name))))


;;; Environment:

(eval-and-compile
  (setq load-path
        (append (delete-dups load-path)
                '("~/.emacs.d/lisp/")))

  (require 'use-package)

  ;;; Make it look nice.
  (set-face-attribute 'default nil
                      :family "Fantasque Sans Mono"
                      :height 130)
  
  
  ;; Theme settings.
  (load-theme 'solarized-dark t)

  ;; Custom file.
  (load (emacs-path "lisp/settings")))


;;; Global packages.
;(require 'git-gutter)
;(global-git-gutter-mode t)


;;; Global IDO Search/Open.
(setq ido-create-new-buffer 'always)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)


;; Programming modes standard preferences:
(use-package magit
  :bind (("<f12>" . magit-status)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


(add-hook 'prog-mode-hook
	  (lambda ()
            (projectile-mode +1)
            (column-enforce-n 80)))


(add-hook 'projectile-mode-hook
	  (lambda ()
	    (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)))


;;; Org-mode Customization.
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-default-notes-file "~/.emacs.d/notes.org")
(setq org-capture-templates
      '(("n" "Note" entry (file+headline "~/.emacs.d/notes.org" "Notes")
         "* %U %?\n\n %i\n %a")
        ("v" "Vacation" entry (file+headline "~/.emacs.d/calendar.org" "Vacation")
         "** %? (,)\n   %t--%t")))


;;; SLIME Customization.
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


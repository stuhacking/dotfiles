;;; smh-kbd-init.el --- various keyboard remappings

;; Use regexp isearch by default.
(global-set-key "\C-s" #'isearch-forward-regexp)
(global-set-key "\C-r" #'isearch-backward-regexp)

;; append to register.
(global-set-key "\C-xx" #'append-to-register)

;; auto-indent on newline in programming modes.
(define-key lisp-mode-map (kbd "RET") #'newline-and-indent)
(define-key emacs-lisp-mode-map (kbd "RET") #'newline-and-indent)

;; Alt-TAB has been stolen by Windows
;; use M-l for lisp function completetion
(global-set-key "\M-l" #'lisp-complete-symbol)

;; Bind key for toggling window zoom
(global-set-key [f11] #'toggle-zoom-window)

;; Add Fortune Command to Help Menu.
(global-set-key [f10] #'cookie)

;; Show Magit Status
(global-set-key [f11] #'magit-status)

;; Bind key to zoom active window
(global-set-key [f12] 'toggle-zoom-window)

;; Bind key to function help.
(global-set-key [M-f1] #'describe-function-at-point)

;; Add Whitespace mode to the minor mode menu.
(define-key mode-line-mode-menu [whitespace-mode]
  '(menu-item "Whitespace" whitespace-mode
              :help (describe-function #'whitespace-mode)
              :button (:toggle . (bound-and-true-p whitespace-mode))))

;;; smh-kbd-init.el ends here

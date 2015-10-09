;; Web Development Customization
;;
; Requires: rainbow-mode, web-mode

;; Code:
(defun web-minor-modes ()
  (rainbow-mode))

(add-hook 'less-css-mode-hook #'web-minor-modes)
(add-hook 'html-mode-hook #'web-minor-modes)
(add-hook 'web-mode-hook #'web-minor-modes)

(add-to-list 'auto-mode-alist '("\\.less" . less-css-mode))
(add-to-list 'auto-mode-alist '("\\.php" . web-mode))

;; init-web ends here.

;; Haskell Specific Customizations
(add-hook 'haskell-mode-hook
          #'(lambda ()
              (turn-on-haskell-doc-mode)
              (turn-on-haskell-indentation)
              (interactive-haskell-mode)
              (setq haskell-process-suggest-remove-import-lines t
                    haskell-process-auto-import-loaded-modules t
                    haskell-process-log t
                    haskell-tags-on-save t)))

(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)

(when (eq system-type 'windows-nt)
  (setq haskell-program-name "ghci.exe"))

;; init-haskell ends here.

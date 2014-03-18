;; Haskell Specific Customizations
(add-hook 'haskell-mode-hook
          #'(lambda ()
              (turn-on-haskell-doc-mode)
              (turn-on-haskell-indentation)))

(when (eq system-type 'windows-nt)
  (setq haskell-program-name "ghci.exe"))

;; init-haskell ends here.

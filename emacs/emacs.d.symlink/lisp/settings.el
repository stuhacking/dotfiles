
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "xelatex")
 '(TeX-command "xetex")
 '(TeX-command-list
   '(("TeX" "%(PDF)%(tex) %(file-line-error) %`%(extraopts) %S%(PDFout)%(mode)%' %(output-dir) %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %T" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %(o-dir) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) %(o-dir) --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "amstex %(PDFout) %`%(extraopts) %S%(mode)%' %(output-dir) %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "%(cntxcom) --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "%(cntxcom) %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "%(bibtex) %s" TeX-run-BibTeX nil
      (plain-tex-mode latex-mode doctex-mode context-mode texinfo-mode ams-tex-mode)
      :help "Run BibTeX")
     ("Biber" "biber %s %(output-dir)" TeX-run-Biber nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-dvips t
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Generate PostScript file")
     ("Dvips" "%(o?)dvips %d -o %f " TeX-run-dvips nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Convert DVI file to PostScript")
     ("Dvipdfmx" "dvipdfmx %d -o %(O?pdf)" TeX-run-dvipdfmx nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Convert DVI file to PDF with dvipdfmx")
     ("Ps2pdf" "ps2pdf %f %(O?pdf)" TeX-run-ps2pdf nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Convert PostScript file to PDF")
     ("Glossaries" "makeglossaries %(O?aux)" TeX-run-command nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Run makeglossaries to create glossary
     file")
     ("Index" "%(makeindex) %s" TeX-run-index nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Run makeindex to create index file")
     ("upMendex" "upmendex %(O?idx)" TeX-run-index t
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Run upmendex to create index file")
     ("Xindy" "texindy %s" TeX-run-command nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")))
 '(TeX-engine 'xetex)
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/sav/" t)))
 '(backup-directory-alist '(("\".\"" . "\"~/.emacs.d/bak/\"")))
 '(before-save-hook nil)
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
   '("~/bin" "~/.npm/node_modules/bin" "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/opt/node/bin" nil))
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
 '(package-selected-packages
   '(org-capture ws-butler restclient yaml-mode git-gutter markdown-mode go-mode doom-themes racket-mode auctex delight diminish use-package powerline c-eldoc org-bullets darkroom paredit column-enforce-mode rust-mode rustic cider htmlize clang-format rainbow-mode rainbow-delimiters magit projectile-speedbar projectile glsl-mode))
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
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(visual-line-fringe-indicators '(left-curly-arrow nil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-mismatch ((t (:background "#dc322f" :foreground "#FFFBF0" :box (:line-width 2 :color "#FFFBF0" :style released-button) :weight ultra-bold)))))

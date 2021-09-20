;;; perl-helper.el --- Provide some assistance in writing perl scripts

;; Copyright (C)  2010  Stuart Hacking <stuhacking@gmail.com>

;; Author: Stuart Hacking <stuhacking@gmail.com>
;; Maintainer: Stuart Hacking <stuhacking@gmail.com>
;; Version: 1.0.0
;; Keywords: tools

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; This file provides a minor mode to assist with writing perl scripts
;; that comply with the strict formatting guidelines. It consists of a
;; number of validation checks, as well as some procedures for
;; generating code.

;;; History:

;;; Code:
(eval-when-compile
  (require 'cl))

;;;;;;;; Customization Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup perl-helper nil
  "Minor mode for assistance writing Perl scripts."
  :prefix "perl-helper-"
  :group 'perl
  :version "23.1")


;;;;;;;; Mode Maps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar perl-helper-mode-menu-bar-map
  (let ((map (make-sparse-keymap)))

    (define-key map [insert insert-sub]
      '(menu-item "Insert Sub" perl-helper-insert-subroutine
		  :help "Insert a new Subroutine"))

    map))

(defun perl-helper-variable-type (var)
  "Function to determine the type of a variable VAR.

Perl supports three types of variable:
  - scalar types (strings, numbers, characters) denoted by '$'
  - array types denoted by '@'
  - lookup types (assoc. array) denoted by '%'."
  (let ((type-marker (car (string-to-list var))))
    (cond ((eq type-marker ?\$) 'scalar)
          ((eq type-marker ?\@) 'array)
          ((eq type-marker ?\%) 'lookup)
          (t 'unknown))))
        
(defun perl-helper-parameters-valid-p (params)
  "Return `t' if all parameters have recognised variable types."
  (= 0 (loop for i in params
             count (equal (perl-helper-variable-type i)
                          'unknown))))

;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun perl-helper-insert-subroutine ()
  "Add a new subroutine to the program.
The name of the subroutine and expected parameters will be read
from the minibuffer. The variables must be valid perl identifiers."
  (interactive)
  ;; Ensure we aren't inserting a sub inside another sub.
  (search-forward-regexp "^}[ 	]*$")
  (beginning-of-line)
  (when (char-equal (following-char) ?\})
      ;; about to crunch another sub
      (forward-char)
      (insert "\n\n"))
  (unless (equal (point) (point-max))
    (kill-line))
  (save-excursion
    (let* ((sub-name (read-from-minibuffer "Name: "))
           (paramstr (read-from-minibuffer "parameters [space delimited]: "))
           (params (split-string paramstr "[ ]+" t)))
      (if (perl-helper-parameters-valid-p params)
          (let ((sub-signature
                 (concat
                  (loop for i in params
                        collect (car (string-to-list i))))))
            (insert (format "sub %s (%s) {\n" sub-name sub-signature))
            (insert (format "\tmy (%s) = @_;\n" (mapconcat #'identity params ", ")))
            (insert (format "\n}\n")))
        (message "Error -- One or more parameters were not valid.")))))


;;;###autoload
(define-minor-mode perl-helper-mode
  "A minor mode for assistance in writing and validating Perl scripts."
  nil
  " Pl-hlp"
  (list (cons [menu-bar] perl-helper-mode-menu-bar-map))
  :group 'perl
  (if perl-helper-mode
      (progn
        ;; Turn off minor mode if major mode changes.
        (add-hook 'change-major-mode-hook
                  (lambda () (perl-helper-mode -1))
                  nil t))))

(provide 'perl-helper)

;;; perl-helper.el ends here

;; testing snippets - remove later
(perl-helper-parameters-valid-p '("$foo" "bar"))

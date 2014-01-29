;;; number-to-word.el --- Convert large numerical strings into English text.

;;  Copyright (C) 2010, Stuart Hacking <stuhacking@gmail.com>

;;  Author: Stuart Hacking <stuhacking@gmail.com>
;;  Maintainer: Stuart Hacking <stuhacking@gmail.com>
;;  Version: 1.0
;;  Created: 10-Aug-2010
;;  URL:

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

;; This file provides number-to-string, which takes a number
;; represented by arabic numerals and converts it into the equivalent
;; English wording.  It was inspired by the discovery that Common Lisp
;; already has this functionality built in.  The code is probably
;; sub-optimal and un-lispy in places but I will continue to clean it
;; up as I learn the Right Way(tm).

;; The following commands are exposed:
;;  - number-to-word-prompt: Read a number from the minibuffer
;;                           and echo the result
;;  - number-to-word-insert: Read a number from the minibuffer
;;                           and insert into the buffer at point
;;  - number-to-word-at-point: Read the word at point and echo
;;                             the result.
;;  - convert-number-to-word-at-point: Read the word at point
;;                                     and replace it with the
;;                                     converted representation.

;; Initially a number is parsed and split into groups of three,
;; following the English convention, thus:
;;    5436234 => (5 436 234)
;;
;; Each three digit group represents a level of thousands, e.g. million,
;; thousand.  Individually, the groups will have a digit, and optionally
;; a ten and hundred unit, e.g.
;;    123 =>
;;        1 hundred
;;        2 ten (twenty)
;;        3 unit
;; If the group is nil, contains only  nil, or contains only 0 (zero) then it can be
;; ignored.
;;
;; Three digit groups can normally be composed very simply by concatenating the words.
;; A special case is the numbers 11..19 which are designated special names.
;;
;; The final output will combine each grouping of hundreds with the appropriate
;; thousand label as follows:
;;    5436234 =>
;;      five million
;;      four hundred thirty six thousand
;;      two hundred thirty four
;;
;; The short scale is used in conversion.
;;  see: http://en.wikipedia.org/wiki/Long_and_short_scales

;;; History:

;; Version 1.0 (10-Aug-2010)
;;   + Created

;;; Code:
(eval-when-compile
  (require 'cl))

(defconst *numbers*
  '((1 . "one")
    (2 . "two")
    (3 . "three")
    (4 . "four")
    (5 . "five")
    (6 . "six")
    (7 . "seven")
    (8 . "eight")
    (9 . "nine")
    (10 . "ten")
    (11 . "eleven")
    (12 . "twelve")
    (13 . "thirteen")
    (14 . "fourteen")
    (15 . "fifteen")
    (16 . "sixteen")
    (17 . "seventeen")
    (18 . "eighteen")
    (19 . "nineteen")
    (20 . "twenty")
    (30 . "thirty")
    (40 . "forty")
    (50 . "fifty")
    (60 . "sixty")
    (70 . "seventy")
    (80 . "eighty")
    (90 . "ninety"))
  "Table of fundamental numeric representations.
Compound numbers are fomed by joining the multiples of 10 with
the digits.  The values between 10 and 19 inclusive are formed
using a special case.")

(defconst *powers-of-ten*
  '("" "thousand" "million" "billion" "trillion" "quadrillion" "quintillion"
    "sextillion" "septillion" "octillion" "nonillion" "decillion" "undecillion"
    "duodecillion" "tredecillion" "quattuordecillion" "quindecillion" "sexdecillion"
    "octodecillion" "novemdecillion" "vigintillion")
  "List of ascending powers of ten.")

(defun number-lookup (number)
  "Look up word representation of NUMBER."
  (cdr (assoc number *numbers*)))

(defun scale-lookup (scale)
  "Look up word representation of SCALE."
  (nth scale *powers-of-ten*))

(defun remove-null-or-empty (list)
  "Drop occurrences of nil or empty string from LIST."
  (remove-if #'(lambda (x)
                 (or (null x)
                     (equal x "")))
             list))

(defun ensure-string (s)
  "Cast s to string."
  (cond ((stringp s) s)
        ((numberp s) (number-to-string s))
        ((symbolp s) (symbol-name s))
        (t (error "Don't know how to create string from S"))))

(defun number-append-hundred (number)
  "Append 'hundred' only when NUMBER is non-nil."
  (let ((stem (number-lookup number)))
    (when stem
      (concat stem " hundred"))))

(defun digit-group-to-word (num level)
  "Convert a three digit group into text.
Convert NUM (a three digit number) into a 'hundred group' and append
the appropriate power-of-ten label, determined by LEVEL."
  (let ((digit-list (loop until (= num 0)
                          collect (mod num 10)
                          do (setq num (/ num 10))))
        (num-words '()))

    (when (third digit-list)
      (push (number-append-hundred (third digit-list))
            num-words))

    (if (and (second digit-list)
             (= (second digit-list) 1))
        ;; Number is in the range 10-19
        ;; Special case
        (push (number-lookup (+ (first digit-list)
                                (* 10 (second digit-list))))
              num-words)
      ;; Numbers can be joined normally
      (progn
        ;; push unit-of-ten into the number if exists.
        (when (second digit-list)
          (push (number-lookup (* 10 (second digit-list)))
                num-words))
        ;; push unit into the number if exists
        (when (first digit-list)
          (push (number-lookup (first digit-list))
                num-words))))

    ;; If we have a number and level then
    ;; add a thousand value.
    (when num-words
      (push (scale-lookup level) num-words))
    (mapconcat #'identity
               (reverse (remove-null-or-empty num-words))
               " ")))

(defun group-digits (number)
  (let ((numvect (reverse (string-to-list (ensure-string number)))))
    (loop while numvect
       collect (concat (remove-if #'null
                                  (list (third numvect)
                                               (second numvect)
                                               (first numvect)))) into groups
         do (setq numvect (cdddr numvect))
       finally (return (reverse groups)))))

(defun digits-to-words (digit-groups)
  (let ((exponent 0))
    (reverse
     (remove-null-or-empty
      (loop for i in (reverse digit-groups)
         collect (digit-group-to-word (string-to-number i) exponent)
         do (incf exponent))))))

(defun number-to-word (number)
  "Convert a number NUMBER into it's textual representation.
Currently, only whole numbers are supported.

Emacs does not support bignums.  You can still pass numeric
arguments, but they will overflow. If you want to convert large
numbers then you can pass them as a string."
  (let* ((digit-list (group-digits number))
         (word-list (digits-to-words digit-list)))
    (if word-list
        (mapconcat #'identity word-list ", ")
        "zero")))

;; Interactive Commands
(defun number-to-word-prompt ()
  "Convert a number (from prompt) to an English representation."
  (interactive)
  (let ((num (read-from-minibuffer "Number: ")))
    (message "%s" (number-to-word num))))

(defun number-to-word-insert ()
  (interactive)
  (let ((num (read-from-minibuffer "Number: ")))
    (insert (format "%s" (number-to-word num)))))

(defun number-to-word-at-point ()
  (interactive)
  (let* ((num (thing-at-point 'word)))
    (message "%s" (number-to-word num))))

(defun convert-number-to-word-at-point ()
  (interactive)
  (let* ((num (thing-at-point 'word))
         (bounds (bounds-of-thing-at-point 'word))
         (start (first bounds))
         (end (cdr bounds)))
    (delete-region start end)
    (insert (format "%s" (number-to-word num)))))

(provide 'number-to-word)
;;; number-to-word.el ends here


;;; wordcount.el --- Simple document statistics for Emacs.
;;; Copyright (C) 2007 - 2010 Stuart Hacking <stuhacking@gmail.com>.

;; Author:   Stuart Hacking <stuhacking@gmail.com>
;; Created:  09-Sep-2007
;; Version:  1.3 (16-Apr-2010)
;; Keywords: word, count, statistics

;; This file is not part of GNU Emacs.

;;; Commentary:

;; It's a pain that currently the only way to do simple things like
;; wordcounting rely on external programs like `wc'. This package
;; fixes that by providing some simple procedures for counting the
;; most common metrics in a document: Words, characters, paragraphs
;; and lines.

;; Call `wordcount-display-statistics' to get a rundown of the current
;; buffer statistics in a snazzy popup window.

;;; History:

;;  Version 1.3 (16-Apr-2010)
;;   + added `wordcount-display-statistics' To display all metrics
;;     in a popup window
;;   + Added Emacs Documentation convention comments
;;  Version 1.2 (06-Dec-2007)
;;   + Combined individual counters into general `count-function-region'
;;     - Does the main loop with step size determined by function.
;;  Version 1.1 (28-Nov-2007)
;;   + Separated counting functions from interactive functions
;;   + Separated Buffer and Region variants
;;  Version 1.0 (09-Sep-2007)
;;   + Simple Buffer Word count, Char count, Sentence Count. Poorly structured.

;;; Code:
(defgroup wordcount nil
  "Wordcount related functionality"
  :version "1.3"
  :group 'editing)

(defun count-function-region (beginning end &optional function)
  "Counts the number of occurences of a pattern of text within the region.
Procedure to count the occurences of a pattern of text within the
region specified by BEGINNING and END.  An optional FUNCTION may be
supplied which will be used to define the step amount.
Reasonable functions include: `forward-char' (default)
`forward-word' `forward-sentence' `forward-paragraph'
`forward-page'

Although any function that alters the position of the POINT is
acceptable."
  (save-excursion
    (or  function (setq function #'forward-char))
    (setq count 0)
    (goto-char beginning)
    (while (< (point) end)
      (funcall function)
      (setq count (1+ count)))
    count))

;; Having all these spurious defuns doesn't feel very lispy
;; TODO: come up with a way to remove this duplication
(defun wordcount-count-chars-region (beginning end)
  "Count chars in region specified by BEGINNING and END."
  (interactive "r")
  (let ((chars (count-function-region beginning end #'forward-char)))
    (message "%s Chars in Region" chars)))


(defun wordcount-count-chars-buffer ()
  "Count chars in buffer."
  (interactive)
  (let ((chars (count-function-region (point-min) (point-max) #'forward-char)))
    (message "%s Chars in Buffer" chars)))

(defun wordcount-count-words-region (beginning end)
  "Count words in region specified by BEGINNING and END."
  (interactive "r")
  (let ((words (count-function-region beginning end #'forward-word)))
    (message "%s Words in Region" words)))


(defun wordcount-count-words-buffer ()
  "Count words in buffer."
  (interactive)
  (let ((words (count-function-region (point-min) (point-max) #'forward-word)))
    (message "%s Words in Buffer" words)))

(defun wordcount-count-sentences-region (beginning end)
  "Count sentences in region specified by BEGINNING and END."
  (interactive "r")
  (let ((sentences (count-function-region beginning end #'forward-sentence)))
    (message "%s Sentences in Region" sentences)))


(defun wordcount-count-sentences-buffer ()
  "Count sentences in buffer."
  (interactive)
  (let ((sentences (count-function-region (point-min) (point-max) #'forward-sentence)))
    (message "%s Sentences in Buffer" sentences)))

(defun wordcount-display-statistics ()
  "Display overall buffer statistics.
Displays useful set of statistics in a new popup buffer."
  (interactive)
  (let* ((words (count-function-region (point-min) (point-max) #'forward-word))
         (chars (count-function-region (point-min) (point-max) #'forward-char))
         (sentences (count-function-region (point-min) (point-max) #'forward-sentence))
         (ave-sentence-len (/ words sentences)))
    (save-excursion
      (pop-to-buffer "*Document Statistics*" t)
      (progn
        (delete-region (point-min) (point-max))
        (insert "Statistics\n==========")
        (newline)
        (insert (format "%s characters in buffer" chars))
        (newline)
        (insert (format "%s words in buffer" words))
        (newline)
        (insert (format "%s sentences in buffer" sentences))
        (newline)
        (newline)
        (insert (format "Average sentence length: %s words." ave-sentence-len))))))

(provide 'wordcount)
;;; wordcount.el ends here

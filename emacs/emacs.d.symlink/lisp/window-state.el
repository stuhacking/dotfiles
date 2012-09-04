;;; window-state.el --- save and restore window configurations
;;; Author:  Stuart Hacking <stuhacking@gmail.com>
;;; Version: 1.0
;;; Created: 20-May-2011

;;; Commentary:

;; Temporarily expand a window to fill a frame. Restore previous window layout when done.

;; TODO: Currently only works on a single frame. Make it save/restore state across multiple frames.

;; TODO: make it possible to copy layouts across frames. (Make it possible to peek into the stack.)

;;; Code:

(defvar *saved-window-configurations* nil)
(defvar *window-zoomed-p* nil)

(defun push-window-configuration ()
  "Store the current `window-configuration'."
  (push (current-window-configuration) *saved-window-configurations*))

(defun pop-window-configuration ()
  "Retrieve the most recently saved `window-configuration'."
  (let ((window-conf (car *saved-window-configurations*)))
    (setq *saved-window-configurations* (cdr *saved-window-configurations*))
    window-conf))

(defun zoom-window ()
  "Expand the active window to fill the frame."
  (interactive)
  (push-window-configuration)
  (delete-other-windows))

(defun restore-last-window-state ()
  "Restore the previous window layout."
  (interactive)
  (setf (current-window-configuration) (pop-window-configuration)))

(defun toggle-zoom-window ()
  "Toggle the expanded state of the active window."
  (interactive)
  (if *window-zoomed-p*
      (progn
        (restore-last-window-state)
        (setq *window-zoomed-p* nil))
    (zoom-window)
    (setq *window-zoomed-p* t)))

;;; window-state.el ends here

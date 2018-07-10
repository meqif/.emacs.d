;;; flymake-diagnostic-at-point-mode.el ---            -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ricardo Martins

;; Author: Ricardo Martins <ricardo@scarybox.net>
;; Keywords: convenience, languages, tools
;; Version: 0.1.0
;; Package-Requires: ((flymake "1.0") (posframe "0.4.1") (emacs "26.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'flymake)
(require 'posframe)

(defcustom flymake-diagnostic-at-point-timer-delay 0.5
  "Delay in seconds before displaying errors at point."
  :group 'flymake-diagnostic-at-point
  :type 'number
  :safe #'numberp)

(defcustom flymake-diagnostic-at-point-error-prefix "➤ "
  "String to be displayed before every error line in the popup.")

(defcustom flymake-diagnostic-at-point-display-diagnostic-function
  'flymake-diagnostic-at-point-display-posframe
  "The function to be used to display the diagnostic message.")

(defvar-local flymake-diagnostic-at-point-timer nil
  "Timer to automatically show the error at point in a popup.")

(defvar-local flymake-diagnostic-at-point-posframe-buffer " *flymake-diagnostic-at-point-posframe-buffer*")

(defun flymake-diagnostic-at-point-get-diagnostic-text ()
  (flymake--diag-text (get-char-property (point) 'flymake-diagnostic)))

(defun flymake-diagnostic-at-point-display-posframe (text)
  (posframe-show flymake-diagnostic-at-point-posframe-buffer
                 :string (concat flymake-diagnostic-at-point-error-prefix text)
                 :background-color (face-background 'popup-face)
                 :foreground-color (face-foreground 'popup-face)
                 :position (point)))

(defun flymake-diagnostic-at-point-maybe-display ()
  (when (and flymake-mode
             (get-char-property (point) 'flymake-diagnostic))
    (with-current-buffer (get-buffer-create flymake-diagnostic-at-point-posframe-buffer)
      (erase-buffer))
    (let ((text (flymake-diagnostic-at-point-get-diagnostic-text)))
      (funcall flymake-diagnostic-at-point-display-diagnostic-function text))
    (add-hook 'pre-command-hook #'flymake-diagnostic-at-point-delete-popup nil t)))

(defun flymake-diagnostic-at-point-delete-popup ()
  (posframe-delete-frame flymake-diagnostic-at-point-posframe-buffer))

;;;###autoload
(defun flymake-diagnostic-at-point-set-timer ()
  (interactive)
  (flymake-diagnostic-at-point-cancel-timer)
  (unless flymake-diagnostic-at-point-timer
    (setq flymake-diagnostic-at-point-timer
          (run-with-idle-timer
           flymake-diagnostic-at-point-timer-delay nil #'flymake-diagnostic-at-point-maybe-display))))

;;;###autoload
(defun flymake-diagnostic-at-point-cancel-timer ()
  "Cancel the error display timer for the current buffer."
  (interactive)
  (let ((inhibit-quit t))
    (when flymake-diagnostic-at-point-timer
      (cancel-timer flymake-diagnostic-at-point-timer)
      (setq flymake-diagnostic-at-point-timer nil))))

(defun flymake-diagnostic-at-point-setup ()
  (add-hook 'focus-out-hook #'flymake-diagnostic-at-point-cancel-timer nil 'local)
  (add-hook 'focus-in-hook #'flymake-diagnostic-at-point-set-timer nil 'local)
  (add-hook 'post-command-hook #'flymake-diagnostic-at-point-set-timer nil 'local))

(defun flymake-diagnostic-at-point-teardown ()
  (remove-hook 'focus-out-hook #'flymake-diagnostic-at-point-cancel-timer 'local)
  (remove-hook 'focus-in-hook #'flymake-diagnostic-at-point-set-timer 'local)
  (remove-hook 'post-command-hook #'flymake-diagnostic-at-point-set-timer 'local))

(define-minor-mode flymake-diagnostic-at-point-mode
  "Minor mode for displaying flymake diagnostics at point in a popup."
  :lighter nil
  :group flymake-diagnostic-at-point
  (cond
   (flymake-diagnostic-at-point-mode
    (flymake-diagnostic-at-point-setup))
   (t
    (flymake-diagnostic-at-point-teardown))))

(provide 'flymake-diagnostic-at-point)
;;; flymake-diagnostic-at-point.el ends here

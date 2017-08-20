;;; faun-mode.el --- Minor mode for automatic pandoc processing  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Ricardo Martins

;; Author: Ricardo Martins
;; Keywords: convenience
;; Package-Requires: ((emacs "24.3") (f "0.19.0"))

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Code:

(eval-when-compile (require 'f))

(defgroup faun nil "Minor-mode for automatic pandoc processing."
  :group 'org)

(defcustom faun-pandoc-output-format "markdown_github"
  "Desired pandoc output format."
  :type 'string
  :group 'faun)

(defcustom faun-pandoc-output-extensions '("footnotes" "yaml_metadata_block")
  "Syntax extensions for the output format."
  :type '(repeat string)
  :group 'faun)

(defun faun-mode--pandoc-after-save ()
  (let ((input-filename (buffer-file-name))
        (output-filename
         (concat "~/ricardomartins.cc/blog/_drafts/"
                 (format-time-string "%Y-%m-%d")
                 "-"
                 (f-base (buffer-file-name))
                 ".md"))
        (output-format
         (s-join "+" (cons faun-pandoc-output-format
                           faun-pandoc-output-extensions))))
    (call-process-shell-command
     (concat "pandoc -s -V layout=post "
             "-t "
             output-format
             " "
             input-filename
             " | sed 's|file://||g' > "
             output-filename)
     nil 0)))

(defun faun-mode-turn-on ()
  (add-hook 'after-save-hook #'faun-mode--pandoc-after-save t t))

(defun faun-mode-turn-off ()
  (remove-hook 'after-save-hook 'faun-mode--pandoc-after-save t))

;;;###autoload
(define-minor-mode faun-mode
  "Getch'er after-save-automatic pandoc processing here"
  :lighter " faun"
  (if faun-mode (faun-mode-turn-on) (faun-mode-turn-off)))

(provide 'faun-mode)
;;; faun-mode.el ends here

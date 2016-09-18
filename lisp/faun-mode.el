(eval-when-compile (require 'f))

(defgroup faun nil "Minor-mode for automatic pandoc processing."
  :group 'org)

(defcustom faun-pandoc-output-format "markdown_github"
  "Desired pandoc output format."
  :group 'faun)

(defcustom faun-pandoc-output-extensions '("footnotes" "yaml_metadata_block")
  "Syntax extensions for the output format."
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

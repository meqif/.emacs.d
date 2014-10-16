(add-hook 'markdown-mode-hook (lambda ()
    ;; Enable word wrapping
    (setq word-wrap t)
    ;; Enable reftex citations
    ;; This is particularly useful when writing papers to convert with pandoc
    (setq reftex-mode)
    (setq reftex-cite-format
        '(
            (?\C-m . "\\cite[]{%l}")
            (?t . "\\textcite{%l}")
            (?a . "\\autocite[]{%l}")
            (?p . "\\parencite{%l}")
            (?f . "\\footcite[][]{%l}")
            (?F . "\\fullcite[]{%l}")
            (?x . "[]{%l}")
            (?X . "{%l}")
            ))

    (defun markdown-toggle-checkbox ()
      "Toggle checkmark on Github Flavored Markdown checklist entry"
      (interactive)
      (let (originalCursor currentLine matchPos)
        (setq originalCursor (point))
        (setq currentLine (thing-at-point 'line))
        (goto-char (line-beginning-position)) ; search from the beginning of the current line
        (setq matchPos (search-forward "[ ]" (line-end-position) t))
        (if (not (equal matchPos nil)) ; did we find an empty checkbox?
            (replace-match "[x]" nil t); fill it!
          (progn (setq matchPos (search-forward "[x]" (line-end-position) t))
                 (if (not (equal matchPos nil)) ; did we find a filled checkbox?
                     (replace-match "[ ]" nil t) ; clear it!
                   (goto-char originalCursor) ; nothing was found, put cursor back in original position
                   )))))
    (define-key markdown-mode-map (kbd "C-c C-c") 'markdown-toggle-checkbox)
))

;; Better headings
;; (set-face-attribute 'markdown-header-face-1 nil :inherit markdown-header-face :height 2.0)
;; (set-face-attribute 'markdown-header-face-2 nil :inherit markdown-header-face :height 1.5)
;; (set-face-attribute 'markdown-header-face-3 nil :inherit markdown-header-face :height 1.25)

(mapcar '(lambda (entry)
           (let ((face-name (car entry))
                 (face-attr (cadr entry))
                 (face-attr-value (car (cddr entry))))
             (set-face-attribute face-name nil
                                 :inherit markdown-header-face
                                 face-attr face-attr-value)))
        '((markdown-header-face-1 :height 2.2)
          (markdown-header-face-2 :height 1.7)
          (markdown-header-face-3 :height 1.5)
          (markdown-header-face-4 :height 1.3)
          (markdown-header-face-5 :height 1.1)))

(provide 'setup-markdown)

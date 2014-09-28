(add-hook 'markdown-mode-hook (lambda ()
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

(provide 'setup-markdown)

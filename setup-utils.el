;;----------------------------------------------------------------------------
;; Comment or uncomment current line or active region
;;----------------------------------------------------------------------------
(defun comment-or-uncomment-region-or-line ()
    "Comment or uncomment the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))


;;----------------------------------------------------------------------------
;; Convert string to CamelCase
;;----------------------------------------------------------------------------
(defun to-camel-case (inputString)
  "Convert string to CamelCase."
  (let (parts)
    (setq parts (split-string inputString "-\\|_"))
    (if (= 1 (length parts))
        (car parts)
        (mapconcat 'upcase-initials parts ""))))

(provide 'setup-utils)

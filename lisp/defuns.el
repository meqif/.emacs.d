;;----------------------------------------------------------------------------
;; Eval and replace expression in buffer
;; Source: https://github.com/magnars/.emacs.d/blob/master/defuns/lisp-defuns.el
;;----------------------------------------------------------------------------
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

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
  "Convert string to CamelCase if it has multiple parts."
  (let (parts)
    (setq parts (split-string inputString "-\\|_"))
    (if (= 1 (length parts))
        (car parts)
        (mapconcat 'upcase-initials parts ""))))

;;----------------------------------------------------------------------------
;; Rename the current file
;; Source: https://github.com/purcell/emacs.d/blob/master/lisp/init-utils.el
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))

;;----------------------------------------------------------------------------
;; Move cursor to indentation or start of line, depending on current position
;;----------------------------------------------------------------------------
(defun meqif/move-to-beginning-of-indentation ()
  "Move cursor to indentation or start of line.

Moves cursor to indentation point or, if already there, to the
beginning of the line."
  (interactive)
  (let ((start-point (point)) indentation-point)
    (save-excursion
      (goto-char (line-beginning-position))
      (skip-chars-forward "\t ")
      (setq indentation-point (point)))
    ;; If at indentation point, jump to beginning of line
    (if (equal start-point indentation-point)
        (goto-char (line-beginning-position))
      ;; Otherwise just to the indentation point
      (goto-char indentation-point))))

(defun meqif/TeX-surround-with-quotes ()
  "Surround current region or word with TeX quotes."
  (interactive)
  (save-excursion
    (let (pos1 pos2 bounds)
      (if (use-region-p)
          (setq pos1 (region-beginning)
                pos2 (region-end))
        (progn
          (setq bounds (bounds-of-thing-at-point 'word)
                pos1 (car bounds)
                pos2 (cdr bounds))))
      (goto-char pos2)
      (insert TeX-close-quote)
      (goto-char pos1)
      (insert TeX-open-quote))))

;; Easy mark popping
(defun meqif/pop-mark ()
  (interactive)
  (set-mark-command '(4)))

(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph] #'endless/fill-or-unfill)

(defun gogo-fullscreen ()
  "Change to fullscreen mode.

Make frame fullscreen and change the default font unless it's
already in fullscreen"
  (interactive)
  (toggle-frame-fullscreen))
(defalias 'toggle-fullscreen 'gogo-fullscreen)

(defun toggle-company-ispell ()
  (interactive)
  (cond
   ((memq 'company-ispell company-backends)
    (setq company-backends (delete 'company-ispell company-backends))
    (message "company-ispell disabled"))
   (t
    (add-to-list 'company-backends 'company-ispell)
    (message "company-ispell enabled!"))))

(defun projectile-or-counsel-find-file ()
  "Find file inside project if inside one, with fallback"
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
    (counsel-find-file)))

(defun promote-demote-window-dwim ()
  "Promote current window to top-left window, or swap top-left window with previously used window"
  (interactive)
  (let ((first-window (car (aw-window-list))))
    ;; If the currently selected window is the first window, then switch to the
    ;; previously used window
    (if (equal (selected-window) first-window)
        (aw-flip-window))
    (aw-swap-window first-window)))

(provide 'defuns)

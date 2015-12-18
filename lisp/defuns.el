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

;; Quick and easy require'ing in eval-after-load
(defmacro load-config (&rest arglist)
  (declare (debug t) (indent 2))
  (cons 'progn
        (--map
         (-let* (((feature filename) it))
           `(eval-after-load ,feature
              `(funcall (function ,(lambda () (require ,filename))))))
         (-partition 2 arglist))))

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

(defun meqif/which-function ()
  "Return current function name based on point."
  (require 'which-func)
  ;; Clean the imenu cache
  ;; Source: http://stackoverflow.com/a/13447080/850756
  (setq imenu--index-alist nil)
  (which-function))

;; Easy mark popping
(defun meqif/pop-mark ()
  (interactive)
  (set-mark-command '(4)))

;; Sourced from http://blog.binchen.org/posts/hello-ivy-mode-bye-helm.html

(defun ivy-imenu-get-candidates-from (alist &optional prefix)
  (cl-loop for elm in alist
           nconc (if (imenu--subalist-p elm)
                     (ivy-imenu-get-candidates-from
                      (cl-loop for (e . v) in (cdr elm) collect
                               (cons e (if (integerp v) (copy-marker v) v)))
                      (concat prefix (if prefix ".") (car elm)))
                   (and (cdr elm) ; bug in imenu, should not be needed.
                        (setcdr elm (copy-marker (cdr elm))) ; Same as [1].
                        (list (cons (concat prefix (if prefix ".") (car elm))
                                    (copy-marker (cdr elm))))))))

(defun ivy-imenu-goto ()
  "Go to buffer position"
  (interactive)
  (let ((imenu-auto-rescan t) items)
    (unless (featurep 'imenu)
      (require 'imenu nil t))
    (setq items (imenu--make-index-alist t))
    (ivy-read "imenu items:"
              (ivy-imenu-get-candidates-from (delete (assoc "*Rescan*" items) items))
              :action (lambda (k) (goto-char k)))))

(provide 'defuns)

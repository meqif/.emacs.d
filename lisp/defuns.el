;;; defuns.el -*- lexical-binding: t; -*-

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
  "Find file inside project if inside one, with fallback."
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
    (counsel-find-file)))

(defun promote-demote-window-dwim ()
  "Promote current window to top-left window, or swap top-left window with previously used window."
  (interactive)
  (let ((first-window (car (aw-window-list))))
    ;; If the currently selected window is the first window, then switch to the
    ;; previously used window
    (if (equal (selected-window) first-window)
        (aw-flip-window))
    (aw-swap-window first-window)))

(defun counsel-rg-dwim ()
  "Run counsel-rg in the current project's root directory, or ask for the root directory otherwise.
If a region is active, it will be used as the initial input for counsel-rg."
  (interactive)
  (let ((initial-input
         (when (use-region-p)
           (buffer-substring-no-properties (region-beginning) (region-end)))))
    (-if-let* (((_ . project-root) (project-current)))
        (counsel-rg initial-input project-root)
      (let ((current-prefix-arg '(4)))
        (call-interactively 'counsel-rg)))))

(defun copy-buffer-name ()
  "Copy the current buffer's file name to the kill ring."
  (interactive)
  (-when-let* ((buffer-name (buffer-file-name)))
    (kill-new buffer-name)))

(defun copy-relative-buffer-name ()
  "Copy the current buffer's relative file name to the kill ring."
  (interactive)
  (-when-let* ((buffer-name (buffer-file-name)))
    (kill-new (f-relative buffer-name (-> (project-current) cdr)))))

(defun closing-bracket-p (char)
  "Returns true if CHAR is a closing bracket: ')', ']', or '}'."
  (memq char '(?\) ?\] ?\})))

(defun meqif/ruby-delete-trailing-comma-before-closing-bracket (_original &rest _args)
  "Delete trailing comma before closing parentheses."
  (when (or (eq major-mode 'ruby-mode)
            (eq major-mode 'enh-ruby-mode))
    (save-excursion
      (end-of-line)
      (backward-char)
      (when (and (closing-bracket-p (char-after))
                 (= ?, (char-before)))
        (delete-char -1)))))

(defun meqif/set-fill-column-to-rubocop-max-line-length ()
  (-when-let* ((max-line-length (meqif/find-rubocop-max-line-length)))
    (setq-local fill-column max-line-length)))

(defun meqif/f-read-if-exists (path &optional coding)
  "Read text with PATH if it exists, using CODING.
CODING defaults to ‘utf-8’.

Return the decoded text as multibyte string."
  (when (f-exists-p path)
    (f-read path coding)))

(defun meqif/extract-rubocop-line-length (raw-file)
  (when (string-match "Metrics/LineLength:\n  Max: \\([0-9]+\\)" raw-file)
      (string-to-number (match-string-no-properties 1 raw-file))))

(defun meqif/find-rubocop-max-line-length ()
  (-some-> (f-join (projectile-project-root) ".rubocop.yml")
           (meqif/f-read-if-exists)
           (meqif/extract-rubocop-line-length)))

(defun meqif/company-candidates ()
  (let ((longest-candidate (-max (-map 'length company-candidates))))
    (mapcar (lambda (candidate)
              (let ((annotation
                     (company-call-backend 'annotation candidate)))
                (if (> (length annotation) 0)
                    (progn
                      (set-text-properties
                       0 (length annotation)
                       '(face success) annotation)
                      (cons (concat candidate (s-repeat (- longest-candidate (length candidate)) " ") "\t\t" annotation) candidate))
                  (cons candidate candidate))))
            company-candidates)))

(defun meqif/counsel-company ()
  "Complete using `company-candidates'."
  (interactive)
  (company-mode 1)
  (unless company-candidates
    (company-complete))
  (when company-point
    (when (looking-back company-common (line-beginning-position))
      (setq ivy-completion-beg (match-beginning 0))
      (setq ivy-completion-end (match-end 0)))
    (ivy-read "company cand: " (meqif/company-candidates)
              :unwind #'company-abort
              :action (lambda (candidate)
                        (ivy-completion-in-region-action (cdr candidate))))))

(defun meqif/indent-or-complete-common ()
  "Indent the current line or region, or complete the common part."
  (interactive)
  (cond
   ((use-region-p)
    (indent-region (region-beginning) (region-end)))
   ((memq indent-line-function
          '(indent-relative indent-relative-maybe))
    (meqif/counsel-company))
   ((let ((old-point (point))
          (old-tick (buffer-chars-modified-tick))
          (tab-always-indent t))
      (call-interactively #'indent-for-tab-command)
      (when (and (eq old-point (point))
                 (eq old-tick (buffer-chars-modified-tick)))
        (meqif/counsel-company))))))

(defun meqif/counsel-grep-or-swiper-dwim ()
  "Call swiper with the currently selected region, if any."
  (interactive)
  (swiper
   (when (use-region-p)
     (buffer-substring-no-properties (region-beginning) (region-end)))))

(defun meqif/kill-buffers ()
  "Kill all buffers except for *Messages* and *scratch*."
  (interactive)
  (-some->> (buffer-list)
            (--remove (string-equal (buffer-name it) "*Messages*"))
            (--remove (string-equal (buffer-name it) "*scratch*"))
            (mapc 'kill-buffer)))

(defun meqif/uuid-v4 ()
  "Generate and insert a UUIDv4 without dashes."
  (interactive)
  (shell-command "uuidgen | tr -d '\n-' | tr -d '\n' | tr '[A-Z]' '[a-z]'" t))

(defun meqif/hide-window ()
  (interactive)
  (shell-command "osascript -e 'tell application \"Finder\" to set visible of process \"Emacs\" to false'" nil))

(provide 'defuns)
;;; defuns.el ends here

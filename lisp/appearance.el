;; Appearance

;; Clean previous themes definitions when loading a theme
(defadvice load-theme
    (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;; Load theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(progn
  (load-theme 'eink t)
  (set-face-attribute 'font-lock-keyword-face nil :foreground "#ab4642")
  (set-face-attribute 'highlight nil :background "#fff1aa")
  ;;(set-face-attribute 'whitespace-line nil :background 'unspecified :foreground "magenta")
  (set-face-attribute 'font-lock-doc-face nil :foreground "#808080"))

;; Mac friendly font
(when window-system
  (setq meqif/default-font "-apple-Fira Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1"
        meqif/fullscreen-font "-apple-Fira Mono-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font meqif/default-font)
  (set-frame-size (selected-frame) 120 38))

(defun gogo-fullscreen ()
  "Change to fullscreen mode.

Make frame fullscreen and change the default font unless it's
already in fullscreen"
  (interactive)
  ;; (if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
  ;; (set-face-attribute 'default nil :font meqif/default-font)
  ;; (set-face-attribute 'default nil :font meqif/fullscreen-font))
  (toggle-frame-fullscreen))

;; Highlight current line
(global-hl-line-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Make show-paren highlight the entire expression
(setq show-paren-style 'expression)

;; Customize mode-line project face
(copy-face 'font-lock-constant-face 'project-face)
(set-face-attribute 'project-face nil :foreground "#19D0FF")
(set-face-attribute 'project-face nil :foreground "#086199")

;; Customize mode-line project branch face
(copy-face 'project-face 'branch-face)
(set-face-attribute 'branch-face nil :bold nil)

;; Customize Projectile mode line
(setq projectile-mode-line
 '(:propertize
   (:eval
    (if (string= (projectile-project-name) (getenv "USER"))
        '(" ")
      (concat " " (projectile-project-name))))
   face project-face))

(defvar meqif-vc-mode-line
  '(" " (:propertize
         ;; Strip the backend name from the VC status information
         (:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
                  (substring vc-mode (+ (length backend) 2))))
         face branch-face))
  "Mode line format for VC Mode.")
(put 'meqif-vc-mode-line 'risky-local-variable t)

(setq-default mode-line-format
              '("%e" mode-line-front-space
                ;; Standard info about the current buffer
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification " " mode-line-position
                ;; Show evil-mode state
                evil-mode-line-tag
                ;; Project information
                (projectile-mode projectile-mode-line)
                ;; Version control information
                (vc-mode meqif-vc-mode-line)
                ;; Misc information
                " "
                mode-line-misc-info
                ;; Finally, the modes
                " " mode-line-modes mode-line-end-spaces
                )
              mode-line-position
              '(" 🐱 "
                (line-number-mode
                 ("%l" (column-number-mode ":%c")))))

;; Tweak display-time mode-line format
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)

;; Enable prettify symbols mode
(global-prettify-symbols-mode +1)

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\):"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(provide 'appearance)

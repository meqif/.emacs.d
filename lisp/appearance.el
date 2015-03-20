;; Appearance

; I like line numbers, thankyouverymuch.
(global-linum-mode 1)

;; Clean previous themes definitions when loading a theme
(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;; Custom tweaks to solarized-theme
(setq
 ;; Make the mode-line high contrast
 solarized-high-contrast-mode-line t
 ;; Fix underscored mode-line
 x-underline-at-descent-line t
 ;; Don't use as much bold
 solarized-use-less-bold t
 ;; Disable variable pitch fonts in Solarized theme
 solarized-use-variable-pitch nil
 ;; Fix the fringe and linum
 solarized-distinct-fringe-background t)

;; Load theme
(require 'solarized)
(load-theme 'solarized-light t)

;; mac friendly font
(when window-system
  (setq magnars/default-font "-apple-Monaco-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1"
        meqif/old-font "-apple-Source Code Pro-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1"
        meqif/default-font "-apple-Fira Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1"
        meqif/fullscreen-font "-apple-Fira Mono-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1"
        magnars/presentation-font "-apple-Monaco-medium-normal-normal-*-28-*-*-*-m-0-iso10646-1")
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

;; Shorten nyan cat bar length
(eval-after-load "nyan-mode" (setq-default nyan-bar-length 16))
(autoload 'nyan-create "nyan-mode") ; Nyan doesn't how to autoload :(

;; Enable nyan cat :3
(nyan-mode)

;; Customize mode-line project face
(copy-face 'font-lock-constant-face 'project-face)
(set-face-attribute 'project-face nil :foreground "#19D0FF")

;; Customize mode-line project branch face
(copy-face 'project-face 'branch-face)
(set-face-attribute 'branch-face nil :bold nil)

;; Customize Projectile mode line
(setq projectile-mode-line
 '(:propertize
   (:eval
    (if (string= (projectile-project-name) (getenv "USER"))
        '(" ¬_¬")
      (concat " " (projectile-project-name))))
   face project-face))

(defvar mein-vc-mode-line
  '(" " (:propertize
         ;; Strip the backend name from the VC status information
         (:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
                  (substring vc-mode (+ (length backend) 2))))
         face branch-face))
  "Mode line format for VC Mode.")
(put 'mein-vc-mode-line 'risky-local-variable t)

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
                (vc-mode mein-vc-mode-line)
                ;; Misc information
                " "
                mode-line-misc-info
                ;; Finally, the modes
                " " mode-line-modes mode-line-end-spaces
                )
              mode-line-position
              '((:eval (when nyan-mode (nyan-create)))
                " "
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

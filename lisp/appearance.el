;; Appearance

;; Clean previous themes definitions when loading a theme
(defadvice load-theme
    (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-spacemacs-theme)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        spaceline-minor-modes-separator " · "
        powerline-default-separator nil)
  (spaceline-toggle-buffer-position-off)
  (spaceline-toggle-hud-off)

  ;; Automatically recompile separators
  (advice-add 'load-theme :after
              #'(lambda (&rest ignored-arguments) (spaceline-compile))))

;; Load theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'nord t)
(spaceline-install)

;; Mac friendly font
(when window-system
  (setq meqif/default-font "-apple-Fira Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1"
        meqif/fullscreen-font "-apple-Fira Mono-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font meqif/default-font)
  (let ((width 128)
        (height 38))
    (set-frame-size (selected-frame) width height)))

;; Highlight current line
(global-hl-line-mode 1)

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

;; Tweak display-time mode-line format
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\):"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(provide 'appearance)

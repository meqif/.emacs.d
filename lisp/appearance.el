;; Appearance

;; Clean previous themes definitions when loading a theme
(defun clear-old-theme (&rest args)
  (mapc #'disable-theme custom-enabled-themes))
(advice-add 'load-theme :before #'clear-old-theme)

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
(load-theme 'gruvbox-dark-hard t)
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

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\):"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(provide 'appearance)

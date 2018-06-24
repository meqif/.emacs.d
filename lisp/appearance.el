;;; appearance.el -*- lexical-binding: t; -*-

(defun clear-old-theme (&rest _)
  "Clean previous themes definitions when loading a theme."
  (mapc #'disable-theme custom-enabled-themes))
(advice-add 'load-theme :before #'clear-old-theme)

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-spacemacs-theme)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        spaceline-minor-modes-separator " · "
        powerline-default-separator nil)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-buffer-position-off)
  (spaceline-toggle-hud-off)

  ;; Automatically recompile separators
  (advice-add 'load-theme :after #'(lambda (&rest _) (spaceline-compile))))

;; Load theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'gruvbox-dark-hard t)

;; Mac friendly font
(defvar meqif/default-font "Fira Mono 15")
(defvar meqif/fullscreen-font "Fira Mono-16")

(when window-system
  (set-fringe-mode '(4 . 4))
  (set-face-attribute 'default nil :font meqif/default-font)
  (add-to-list 'default-frame-alist '(width . 132))
  (add-to-list 'default-frame-alist '(height . 32))

  (set-face-attribute 'powerline-active0 nil :height 0.9)
  (set-face-attribute 'powerline-active1 nil :height 0.9)
  (set-face-attribute 'powerline-active2 nil :height 0.9)
  (set-face-attribute 'powerline-inactive0 nil :height 0.9)
  (set-face-attribute 'powerline-inactive1 nil :height 0.9)
  (set-face-attribute 'powerline-inactive2 nil :height 0.9)
  (set-face-attribute 'mode-line nil :height 0.9)
  (set-face-attribute 'mode-line-inactive nil :height 0.9)

  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; Highlight current line
(global-hl-line-mode 1)

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\):"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(provide 'appearance)
;;; appearance.el ends here

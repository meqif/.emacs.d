;;; appearance.el -*- lexical-binding: t; -*-

(defun clear-old-theme (&rest _)
  "Clean previous themes definitions when loading a theme."
  (mapc #'disable-theme custom-enabled-themes))
(advice-add 'load-theme :before #'clear-old-theme)

;; Load theme
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t))

(when window-system
  (set-fringe-mode '(4 . 4))

  ;; Use font with ligatures and enable them in Emacs-Mac
  (when (eq window-system 'mac)
    (mac-auto-operator-composition-mode))

  (when (equal "caffeine" (system-name))
    (set-face-attribute 'default nil :font "Fira Code Tweak 14")
    (add-to-list 'default-frame-alist '(width . 216))
    (add-to-list 'default-frame-alist '(height . 43)))

  (when (equal "ophiuchus" (system-name))
    (set-face-attribute 'default nil :font "Fira Code Tweak 15"))

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

;;; appearance.el -*- lexical-binding: t; -*-

(defun clear-old-theme (&rest _)
  "Clean previous themes definitions when loading a theme."
  (mapc #'disable-theme custom-enabled-themes))
(advice-add 'load-theme :before #'clear-old-theme)

(defun shrink-modeline-font (&rest _)
  (let ((height (round (* (face-attribute 'default :height) 0.85))))
    (set-face-attribute 'mode-line nil :height height)
    (set-face-attribute 'mode-line-inactive nil :height height)))
(advice-add 'load-theme :after #'shrink-modeline-font)

;; Load theme
(use-package doom-themes
  :config
  (load-theme 'doom-oceanic-next t))

(when window-system
  (set-fringe-mode '(4 . 4))

  ;; Vertically center text in line when `line-spacing' is changed
  ;; Requires this patch: https://lists.gnu.org/archive/html/emacs-devel/2019-08/msg00662.html
  (setq-default line-spacing-vertical-center t)

  ;; Use font with ligatures and enable them in Emacs-Mac
  (when (eq window-system 'mac)
    (mac-auto-operator-composition-mode))

  (when (equal "caffeine" (system-name))
    (set-face-attribute 'default nil :font "Fira Code Tweak 14")
    (add-to-list 'default-frame-alist '(width . 216))
    (add-to-list 'default-frame-alist '(height . 43)))

  (when (or (equal "antares.local" (system-name))
            (equal "ophiuchus.local" (system-name)))
    (progn
      (set-face-attribute 'default nil :font "Jetbrains Mono 15")
      (set-face-attribute 'variable-pitch nil :font "Adobe Caslon Pro 18")
      (setq default-text-properties '(line-spacing 0.5)))
    (toggle-frame-maximized))

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

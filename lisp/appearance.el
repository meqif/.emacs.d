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
(use-package modus-themes
  :config
  (setq modus-themes-operandi-color-overrides
        '((bg-main . "#fefcf4")
          (bg-dim . "#faf6ef")
          (bg-alt . "#f7efe5")
          (bg-hl-line . "#f4f0e3")
          (bg-active . "#e8dfd1")
          (bg-inactive . "#f6ece5")
          (bg-region . "#c6bab1")
          (bg-header . "#ede3e0")
          (bg-tab-bar . "#dcd3d3")
          (bg-tab-active . "#fdf6eb")
          (bg-tab-inactive . "#c8bab8")
          (fg-unfocused . "#55556f"))
        modus-themes-vivendi-color-overrides
        '((bg-main . "#100b17")
          (bg-dim . "#161129")
          (bg-alt . "#181732")
          (bg-hl-line . "#191628")
          (bg-active . "#282e46")
          (bg-inactive . "#1a1e39")
          (bg-region . "#393a53")
          (bg-header . "#202037")
          (bg-tab-bar . "#262b41")
          (bg-tab-active . "#120f18")
          (bg-tab-inactive . "#3a3a5a")
          (fg-unfocused . "#9a9aab")))

  (with-eval-after-load 'org
    (set-face-attribute 'org-done nil :inverse-video t)
    (set-face-attribute 'org-todo nil :inverse-video t))

  (defun meqif/theme-changer (appearance)
    (interactive)
    (cond ((eq appearance 'dark) (modus-themes-load-vivendi))
          ((eq appearance 'light) (modus-themes-load-operandi))))

  (if (eq window-system 'ns)
      (add-to-list 'ns-system-appearance-change-functions #'meqif/theme-changer)
    (modus-themes-load-operandi)))

(when window-system
  (set-fringe-mode '(4 . 4))

  ;; Use font with ligatures and enable them in Emacs-Mac
  (when (eq window-system 'mac)
    (mac-auto-operator-composition-mode))

  (set-face-attribute 'default nil :font "Jetbrains Mono 15")
  (set-face-attribute 'variable-pitch nil :font "Georgia 18"))

;; Highlight current line
(global-hl-line-mode 1)

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\):"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

;; *** Ligatures
;; These are the ligatures for the JetBrains Mono font. Since =composite.el= is
;; quite a new feature, I had to build this list myself. The format for this
;; ligature alist and the way that =composition-function-table= works, is that the
;; car for each cons cell is the first character of the ligature sequence, and the
;; cdr is a regexp that matches ligatures that starts with that character. If you
;; have a list of supported ligatures for a font, it's really easy to build these
;; regexps using the =regexp-opt= function.
;; Taken from: https://github.com/robbert-vdh/dotfiles/blob/master/user/emacs/.config/doom/config.org#ligatures
;; Thank you, Robbert!
(let ((alist '((?! . "\\(?:!\\(?:==\\|[!=]\\)\\)")
               (?# . "\\(?:#\\(?:###?\\|_(\\|[!#(:=?[_{]\\)\\)")
               (?$ . "\\(?:\\$>\\)")
               (?& . "\\(?:&&&?\\)")
               (?* . "\\(?:\\*\\(?:\\*\\*\\|[/>]\\)\\)")
               (?+ . "\\(?:\\+\\(?:\\+\\+\\|[+>]\\)\\)")
               (?- . "\\(?:-\\(?:-[>-]\\|<<\\|>>\\|[<>|~-]\\)\\)")
               (?. . "\\(?:\\.\\(?:\\.[.<]\\|[.=?-]\\)\\)")
               (?/ . "\\(?:/\\(?:\\*\\*\\|//\\|==\\|[*/=>]\\)\\)")
               (?: . "\\(?::\\(?:::\\|\\?>\\|[:<-?]\\)\\)")
               (?\; . "\\(?:;;\\)")
               (?< . "\\(?:<\\(?:!--\\|\\$>\\|\\*>\\|\\+>\\|-[<>|]\\|/>\\|<[<=-]\\|=\\(?:=>\\|[<=>|]\\)\\||\\(?:||::=\\|[>|]\\)\\|~[>~]\\|[$*+/:<=>|~-]\\)\\)")
               (?= . "\\(?:=\\(?:!=\\|/=\\|:=\\|=[=>]\\|>>\\|[=>]\\)\\)")
               (?> . "\\(?:>\\(?:=>\\|>[=>-]\\|[]:=-]\\)\\)")
               (?? . "\\(?:\\?[.:=?]\\)")
               (?\[ . "\\(?:\\[\\(?:||]\\|[<|]\\)\\)")
               (?\ . "\\(?:\\\\/?\\)")
               (?\] . "\\(?:]#\\)")
               (?^ . "\\(?:\\^=\\)")
               (?_ . "\\(?:_\\(?:|?_\\)\\)")
               (?{ . "\\(?:{|\\)")
               (?| . "\\(?:|\\(?:->\\|=>\\||\\(?:|>\\|[=>-]\\)\\|[]=>|}-]\\)\\)")
               (?~ . "\\(?:~\\(?:~>\\|[=>@~-]\\)\\)"))))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(provide 'appearance)
;;; appearance.el ends here

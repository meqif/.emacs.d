;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Tabs
(setq-default tab-width 4)

;; Disable backup
(setq backup-inhibited t)

;; Disable auto save
(setq auto-save-default nil)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
;; Make fill-paragraph (Alt-Q and gq) respect the line width
(set-default 'fill-column 80)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Ensure there's an empty line at the end of the file
(setq-default require-final-newline t)

;; Destroy trailing whitespace on exit
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 20000000)

;; Default directory
(setq default-directory "~/")


;; Disable audible bell
(setq visible-bell t)

(provide 'sane-defaults)

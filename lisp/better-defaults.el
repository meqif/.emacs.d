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
;; Actually, let them reach 120 characters, we're living in the far future
;; Make fill-paragraph (Alt-Q and gq) respect the line width
(setq-default fill-column 120)

;; Never insert tabs
(setq-default indent-tabs-mode nil)

;; Ensure there's an empty line at the end of the file
(setq-default require-final-newline t)

;; Destroy trailing whitespace on exit
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Default directory
(setq default-directory "~/")

;; Disable the bell and the flashing
(setq ring-bell-function 'ignore)

;; Enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; Keyboard smooth scrolling: Prevent the awkward "snap to re-center" when
;; the text cursor moves off-screen. Instead, only scroll the minimum amount
;; necessary to show the new line. (A number of 101+ disables re-centering.)
(setq scroll-conservatively 101)

;; Make the minibuffer prompt intangible to stop it from being selectable or
;; navigable with the movement keys
;; Source: https://lists.gnu.org/archive/html/emacs-devel/2016-04/msg00857.html
(unless (version< emacs-version "25.0")
  (let ((default (eval (car (get 'minibuffer-prompt-properties 'standard-value))))
        (dont-touch-prompt-prop '(cursor-intangible t)))
    (setq minibuffer-prompt-properties (append default dont-touch-prompt-prop))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)))

;; Always ask before exiting Emacs
(setq confirm-kill-emacs 'yes-or-no-p)

;; Use https for package archives
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(let ((trustfile (expand-file-name "~/.emacs.d/cacert.pem")))
  (unless (file-exists-p trustfile)
    (error (concat "No certificate bundle file found! "
                   "Please download it with "
                   (format "`curl https://curl.haxx.se/ca/cacert.pem -o %s`" trustfile))))
  (setq tls-checktrust 'always
        tls-program (list (format "gnutls-cli --x509cafile %s -p %%p %%h" trustfile))
        gnutls-verify-error t
        gnutls-trustfiles (list trustfile)))

(provide 'better-defaults)

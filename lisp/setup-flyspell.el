;; Use Aspell for spellcheck
(setq ispell-program-name "~/homebrew/bin/aspell")
(setq ispell-list-command "--list")

;; Default language is Portuguese.
(setq ispell-dictionary "pt_PT")

;; Flyspell messages slow down the spellchecking process
(setq flyspell-issue-message-flag nil)

(provide 'setup-flyspell)

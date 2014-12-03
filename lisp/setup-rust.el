(add-hook 'rust-mode-hook
          (lambda ()
            ;; Enable on-the-fly syntax checking
            (flycheck-mode 1)
            ;; Fix paths where to look for libraries
            (add-to-list 'flycheck-rust-library-path
                         (concat (projectile-project-root) "target"))
            (add-to-list 'flycheck-rust-library-path
                         (concat (projectile-project-root) "target/release"))
            ;; Rust has different rules for too long lines
            (setq-local fill-column 101)
            (setq-local whitespace-line-column 100)
            ;; Reload whitespace mode to make the previous change effective
            (whitespace-mode -1)
            (whitespace-mode 1)))

;; Add brackets to smartparens pair list
(sp-local-pair 'rust-mode "<" ">")

(provide 'setup-rust)

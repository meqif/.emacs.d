;; Make uniquify rename buffers like in path name notation
(require 'uniquify)
(eval-after-load "uniquify"
  '(setq uniquify-buffer-name-style 'forward))

(provide 'setup-uniquify)

;; Reduce how often garbage collection runs during startup
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(setq file-name-handler-alist nil)

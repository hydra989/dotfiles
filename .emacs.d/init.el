;;; init.el


;; performance things
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(org-babel-load-file ".emacs.d/config.org")
(server-start)

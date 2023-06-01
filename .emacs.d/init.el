;;; init.el


;; performance things
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; TODO: move to within ~/.emacs.d for cleanliness reasons
(add-to-list `load-path (expand-file-name "~/elisp"))

;; modified from https://github.com/CeleritasCelery/emacs.d/
;; (let (
;; 	(el-file (expand-file-name "config.el" user-emacs-directory))
;; 	(org-file (expand-file-name "config.org" user-emacs-directory)))
;; (if (and (file-exists-p el-file)
;; 		 (file-newer-than-file-p el-file org-file))
;; 	(load-file el-file)
;;   (require 'org)
;;   (org-babel-load-file org-file)))

;; the nuclear option
(org-babel-load-file "/home/hydra/s/dotfiles/.emacs.d/config.org")

(server-start)

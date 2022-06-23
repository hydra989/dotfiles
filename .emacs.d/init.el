;;; init.el
;;;
;;;		TODO: latex previews?


;; performance things
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; customization
(defvar *theme-magic-enabled* t)	;; true for pywal environments
(defvar *transparency* t)
(defvar *server* t)
(defvar *exwm* nil)

;; reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(let ((file-name-handler-alist nil))
  ;; init melpa so packages.init.el doesn't throw a fit
  (package-initialize)
  (setq package-check-signature nil)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

  ;; https://www.emacswiki.org/emacs/DotEmacsModular
  (defconst hydra:emacs-config-dir "/home/hydra/.emacs.d/src/" "")
  (defun hydra:load-config-file (filelist)
	(dolist (file filelist)
      (load (expand-file-name
			 (concat hydra:emacs-config-dir file)))
      ))
  (hydra:load-config-file '("defaults.init.el"
							"packages.init.el"
							"func.init.el"
							"org-anno.init.el"
							))

  ;; transparancy
  (when *transparency*
	(set-frame-parameter (selected-frame) 'alpha '(96 . 90))
	(add-to-list 'default-frame-alist '(alpha . (82 . 77))))

  ;; general appearance
  (load-theme 'cyberpunk t)
  (add-to-list 'default-frame-alist '(font . "Terminus-11"))

  ;; seperate custom file
  (setq custom-file "/home/hydra/.emacs.d/custom.el")
  (when (file-exists-p custom-file)
	(load custom-file)))

;; start the server
(when *server*
  (server-start))

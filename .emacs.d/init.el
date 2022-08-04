;;; init.el


;; performance things
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(defvar *theme-magic-enabled* (getenv "EMACS_PYWAL"))
(defvar *transparency*        (getenv "EMACS_TRANSPARENCY"))
(defvar *server*              (getenv "EMACS_SERVER"))
(defvar *exwm*                (getenv "EMACS_EXWM"))
(defvar *hostname*            (getenv "HOSTNAME"))

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
  (hydra:load-config-file '(
							"packages.init.el"
							"func.init.el"
							"lsp.init.el"
							"org-anno.init.el"
							"defaults.init.el"
							))

  ;; general appearance
  (defun gui-init (frame)
	(select-frame frame)
	(set-frame-font "Terminus-11" t t)
	(mini-modeline-mode t)
	(load-theme 'cyberpunk t)
	(dashboard-setup-startup-hook))

  (if (daemonp)
	  (add-hook 'after-make-frame-functions #'gui-init)
	(gui-init))

  ;; transparancy
  (if (string-equal *transparency* "y")
	  (set-frame-parameter (selected-frame) 'alpha '(95 . 90))
	  (add-to-list 'default-frame-alist '(alpha . (80 . 75))))

  ;; seperate custom file
  (setq custom-file "/home/hydra/.emacs.d/custom.el")
  (when (file-exists-p custom-file)
	(load custom-file)))

;; start the server
(if (string-equal *server* "y")
  (server-start))

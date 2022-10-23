;;; init.el


;; performance things
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(defvar *theme-magic-enabled* (getenv "EMACS_PYWAL"))
(defvar *transparency*        (getenv "EMACS_TRANSPARENCY"))
(defvar *server*              (getenv "EMACS_SERVER"))
(defvar *exwm*                (getenv "EMACS_EXWM"))
(defvar *hostname*            (getenv "HOSTNAME"))

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
(if (string-equal *exwm* "y")
  (hydra:load-config-file '( "exwm.init.el" )))

(defun daemon-gui-setup (frame)
  (select-frame frame)
  (load-theme 'cyberpunk t)
  (mini-modeline-mode t)
  (set-frame-font "Terminus-11" t t)
  )
(defun standard-setup ()
  (load-theme 'cyberpunk t)
  (mini-modeline-mode t)
  (set-frame-font "Terminus-11" t t)
  )

(if (daemonp)
    (add-hook 'after-make-frame-functions #'daemon-gui-setup)
  (standard-setup))

;(if (daemonp)
;	(add-hook 'after-make-frame-functions #'gui-init-server)
;  (gui-init))

;; transparancy
(if (string-equal *transparency* "y")
  (set-frame-parameter (selected-frame) 'alpha '(95 . 90))
  (add-to-list 'default-frame-alist '(alpha . (80 . 75))))

;; seperate custom file
(setq custom-file "/home/hydra/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(custom-set-variables
 '(org-directory "~/s/org")
 '(org-agenda-files (list "~/s/org/agenda")))

;; start the server
(if (string-equal *server* "y")
  (server-start))

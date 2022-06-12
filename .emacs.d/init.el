;;; init.el
;;;
;;;		TODO: unify style throughout
;;;		TODO: latex previews?
;;;		TODO: fallback font for hack?


;; performance things
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; customization
(defvar *theme-magic-enabled* t)	;; true for pywal environments
(defvar *transparency* t)

;; https://www.emacswiki.org/emacs/LoadingLispFiles
(defun load-directory (dir)
  (let ((load-it (lambda (f)
					 (load-file (concat (file-name-as-directory dir) f)))
				   ))
	(mapc load-it (directory-files dir nil "\\.el$"))))
(load-directory "~/.emacs.d/src")

;; transparancy
(when *transparency*
  (set-frame-parameter (selected-frame) 'alpha '(96 . 90))
  (add-to-list 'default-frame-alist '(alpha . (82 . 77))))

;; general appearance
(load-theme 'cyberpunk t)
(add-to-list 'default-frame-alist '(font . "Terminus-11"))

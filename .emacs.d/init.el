;;; init.el
;;;
;;;		TODO: unify style throughout
;;;		TODO: latex previews?
;;;		TODO: fallback font for hack?


;; performance things
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(defvar *theme-magic-enabled* t)	;; true for pywal environments
(defvar *transparency* nil)

;; https://www.emacswiki.org/emacs/DotEmacsModular
(defconst emacs-config-directory "~/.emacs.d/" "")
(setq custom-file "src/custom")
(defun load-config-file (filelist)
  (dolist (file filelist)
    (load (expand-file-name 
           (concat emacs-config-directory file)))
     ;;(message "Loaded config file:%s" file)
    ))
;; packages.init is loaded first so defaults can be changed post-package loading
(load-config-file '(
		    "src/packages.init"
		    "src/func.init"
		    "src/default.init"
		    "src/org-anno.init"
		    custom-file
		    ))

;; transparancy
(when *transparency*
  (set-frame-parameter (selected-frame) 'alpha '(85 . 75))
  (add-to-list 'default-frame-alist '(alpha . (85 . 75))))

(load-theme 'monokai-pro t)
(set-frame-font "Hack-11.5" nil t)

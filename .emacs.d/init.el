;;; init.el
;;;
;;;		TODO: latex previews?


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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(emojify yasnippet yaml-mode writeroom-mode which-key use-package undo-fu treemacs-magit treemacs-evil treemacs-all-the-icons tree-sitter-langs theme-magic org-noter nix-mode monokai-pro-theme magit-todos lua-mode lsp-ui lsp-treemacs linum-relative ibuffer-vc go-mode fountain-mode flycheck feebleline evil-snipe evil-collection dtrt-indent dockerfile-mode dashboard cyberpunk-theme counsel company-quickhelp company-box base16-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

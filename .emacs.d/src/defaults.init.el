;;; defaults.init.el


;;; modes
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(fringe-mode 1)
(global-auto-revert-mode 1)

;; fighting emacs tab defaults
(setq-default indent-tabs-mode t)
(setq-default tab-always-indent 't)
(setq-default tab-width 4)

;; https://www.victorquinn.com/emacs-prevent-autosave-mess
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; general settings
(setq
 backward-delete-char-untabify-method 'hungry
 make-backup-files nil
 create-lockfiles nil
 ring-bell-function 'ignore
 use-dialog-box nil
 echo-keystrokes 0.2
 vc-follow-symlinks t
 load-prefer-newer t
 delete-old-versions t
 use-short-answers t
 backup-by-copying t			; don't delink hardlinks (?)
 scroll-preserve-screen-position t
 scroll-conservatively 101		; smooth scrolling
 )

(fset 'yes-or-no-p 'y-or-n-p)

;;; language specific settings
(setq
 ; c
 c-default-style "linux"
 )

;;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-show-empty-filter-groups nil
	  ibuffer-expert t
	  ;; a few of these are redundant---
	  ;; ibuffer gives me problems with consistency
	  ibuffer-saved-filter-groups
	  '(("home"
		 ("Dired" (mode . dired-mode))
		 ("Magit" (or
				   (mode . magit-mode)
				   (mode . magit-revision-mode)
				   (mode . magit-diff-mode)
				   (mode . magit-process-mode))
		 ("Writing" (or
					 (mode . fountain-mode)
					 (mode . latex-mode)
					 (mode . markdown-mode)))
		 ("Org"  (mode . org-mode))
		 ("Code" (or
				  (filename . "Git")
				  (mode . prog-mode)
				  (mode . lsp-mode)
				  (mode . nix-mode)))
		 ("Emacs" (or
				   (name . "^\\*scratch\\*$")
				   (name . "^\\*Messages\\*$")
				   (name . "^\\*Warnings\\*$")
				   (name . "^\\*GNU Emacs\\*$")
				   (mode . emacs-lisp-mode)))
		 ("Shell" (or
				   (mode . term-mode)
				   (mode . eshell-mode)))
		 ("LSP" (or
				 (filename . "^\\*pylsp\\*$")
				 (filename . "^\\*lsp-log\\*$")
				 (filename . "^\\*pylsp::stderr\\*$")))
		 )))
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-auto-mode 1)
	    (ibuffer-switch-to-saved-filter-groups "home")))

;; windmove for S-{arrow} window movements
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; dired
(setq dired-kill-when-opening-new-dired-buffer t)

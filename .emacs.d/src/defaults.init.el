;;; defaults.init.el

;;; modes
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(fringe-mode 1)
(global-visual-line-mode 1)
(display-time-mode 1)
(setq display-time-default-load-average nil)
(recentf-mode -1)

;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; fighting emacs tab defaults
(global-set-key (kbd "TAB") 'tab-to-tab-stop)
(setq indent-tabs-mode t)
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
 mouse-autoselect-window t
 focus-follows-mouse t
 echo-keystrokes 0.2
 vc-follow-symlinks t
 load-prefer-newer t
 delete-old-versions t)

;;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-show-empty-filter-groups nil
	  ibuffer-expert t
	  ibuffer-saved-filter-groups
	  '(("home"
		 ("Dired" (mode . dired-mode))
		 ("Magit" (mode . magit))
		 ("Code" (or (filename . "Git")
					 (mode . elisp-mode)
					 (mode . yas-mode)
					 (mode . company-mode)))
		 ("Org"  (mode . org-mode))
		 ("Writing" (or (mode . fountain-mode)
						(mode . latex-mode)
						(mode . markdown-mode)))
		 ("Emacs" (or (name . "^\\*scratch\\*$")
					  (name . "^\\*Messages\\*$")
					  (name . "^\\*Warnings\\*$")
					  (name . "^\\*GNU Emacs\\*$")))
		 ("EXWM" (mode . exwm-mode))
		 ("Shell" (or (mode . vterm-mode)
					  (mode . eshell-mode)))
		 ("LSP" (or (filename . "^\\*pyls\\*$")
					(filename . "^\\*lsp-log\\*$")
					(filename . "^\\*pyls::stderr\\*$")))
		 )))
(add-hook 'ibuffer-mode-hook
		  (lambda ()
			(ibuffer-auto-mode 1)
			(ibuffer-switch-to-saved-filter-groups "home")))

;; windmove for S-{arrow} window movements
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

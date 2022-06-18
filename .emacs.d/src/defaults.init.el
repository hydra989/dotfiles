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
 frame-inhibit-implied-resize t ; shouldn't wildly resize during startup
 frame-resize-pixelwise t
 )

(fset 'yes-or-no-p 'y-or-n-p)

;; language specific settings
(setq
 ; c
 c-default-style "linux"
 )

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; windmove for S-{arrow} window movements
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; dired
(setq dired-kill-when-opening-new-dired-buffer t)

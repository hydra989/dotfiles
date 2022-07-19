;;; packages.init.el


;;; use-package
(unless package-archive-contents
  (package-refresh-contents))
(setq package-native-compile t)
(require 'package)


;;; general
(use-package avy
  :ensure t
  :after evil
  :config
  (evil-define-key 'normal 'global (kbd "SPC") 'avy-goto-char))
(use-package bufler
  :ensure t
  :init
  ;; default bufler config with exwm group
  (bufler-defgroups
   (group
    (auto-workspace))
   (group
    (group-or "*Help/Info*"
              (mode-match "*Help*" (rx bos "help-"))
              (mode-match "*Info*" (rx bos "info-"))))
   ;; TODO: test this ---
   (group
    (mode-match "*EXWM*" (rx bos "exwm-")))
   ;; -------------------
   (group
    (group-and "*Special*"
               (lambda (buffer)
                 (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                      buffer)
                             (funcall (mode-match "Dired" (rx bos "dired"))
                                      buffer)
                             (funcall (auto-file) buffer))
                   "*Special*")))
    (group
     (name-match "**Special**"
                 (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
    (group
     (mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
     (auto-directory))
    (mode-match "*Helm*" (rx bos "helm-"))
    (auto-mode))
   (dir user-emacs-directory)
   ;(group
   ; ; Subgroup collecting buffers in `org-directory' (or "~/org" if
   ; ; `org-directory' is not yet defined).
   ; (dir (if (bound-and-true-p org-directory)
   ;          org-directory
   ;        "~/org"))
   ; (group
   ;   (auto-indirect)
   ;   (auto-file))
   ; (group-not "*special*" (auto-file))
   ; (auto-mode))
   (group
    (auto-project))
   (auto-directory)
   (auto-mode))
   :config
   (global-set-key (kbd "C-x C-b") 'bufler))
(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  ;; Go back home? Just press `bh'
  (dirvish-bookmark-entries
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  (dirvish-header-line-format '(:left (path) :right (free-space)))
  (dirvish-mode-line-format
   '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  (dirvish-attributes '(all-the-icons file-size collapse subtree-state vc-state git-msg))
  :config
  (dirvish-peek-mode)
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  (setq dired-mouse-drag-files t)                   ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  :bind
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map
   ("h" . dired-up-directory)
   ("j" . dired-next-line)
   ("k" . dired-previous-line)
   ("l" . dired-find-file)
   ("i" . wdired-change-to-wdired-mode)
   ("." . dired-omit-mode)
   ("b"   . dirvish-bookmark-jump)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("TAB" . dirvish-subtree-toggle)
   ("M-n" . dirvish-history-go-forward)
   ("M-p" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-f" . dirvish-toggle-fullscreen)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))
(use-package linum-relative
  :ensure t
  :defer t
  :hook (prog-mode . linum-relative-mode)
  :init
  (setq linum-relative-backend 'display-line-numbers-mode))

;; magit
(use-package magit
  :ensure t
  :config
  (setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer (if (and (derived-mode-p 'magit-mode)
                         (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-stash-mode
                                 magit-status-mode)))
                    nil
                  '(display-buffer-same-window))))))
  (with-eval-after-load 'magit-mode
	(add-hook 'after-save-hook 'magit-after-save-refresh-status t))
(use-package magit-todos
  :ensure t
  :after magit
  :config
  (setq magit-todos-ignored-keywords '(""))
  (magit-todos-mode))

;; evil-mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :config
  ;; some modes are better off without evil
  (evil-set-initial-state 'bufler-list-mode 'emacs)
  (evil-set-initial-state 'dirvish-mode 'emacs)
  
  (evil-set-leader 'normal (kbd ";"))
  (evil-mode))
;;(use-package evil-commentary
;;  :ensure t
;;  :after evil
;;  :config
;;  (add-hook 'prog-mode-hook 'evil-commentary-mode))
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))
(use-package evil-snipe
  :ensure t
  :after evil
  :config
  (evil-snipe-mode +1))
(use-package undo-fu
  :ensure t
  :after evil
  :config
  (evil-define-key 'normal 'global "\C-r" 'evil-redo))

;; appearance
(if (string-equal *theme-magic-enabled* "y")
  (use-package theme-magic
    :ensure t
    :config
    (theme-magic-export-theme-mode)))
(use-package base16-theme
  :ensure t
  :defer t)
(use-package cyberpunk-theme
  :ensure t
  :defer t)
(use-package monokai-pro-theme
  :ensure t
  :defer t)
(use-package feebleline
  :ensure t
  :config
  (setq feebleline-msg-functions
		'((feebleline-line-number		    :post "" :fmt "%4s")
		  (feebleline-column-number		    :pre ":" :fmt "%-2s")
		  (feebleline-file-directory        :face feebleline-dir-face :post "")
		  (feebleline-file-or-buffer-name	:face font-lock-warning-face :post "")
		  (feebleline-file-modified-star	:face font-lock-warning-face :post "")
		  (magit-get-current-branch		    :face feebleline-git-face :pre " -> ")
		  ))
  (feebleline-mode 1))
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))
(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))
(use-package dashboard
  :ensure t
  :after counsel-projectile
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-set-init-info nil
        dashboard-set-heading-icons t
        dashboard-center-content t
        dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name
        dashboard-items '((projects . 10)
                          )
        ))

;; ivy
(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t
	enable-recursive-minbuffers t)
  :config
  (ivy-mode))
(use-package counsel
  :ensure t
  :after ivy
  :config
  (counsel-mode))
(use-package swiper
  :ensure t
  :defer t
  :after ivy
  :config
  (global-set-key "\C-s" 'swiper))

;; projectile
(use-package projectile
  :ensure t)
(use-package counsel-projectile
  :ensure t
  :after projectile
  :config
  (counsel-projectile-mode))

;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (global-set-key (kbd "C-c t") 'treemacs)
  :config
  (setq-default treemacs-use-follow-mode nil
				treemacs-use-filewatch-mode t
				treemacs-use-git-mode 'deferred))
(use-package treemacs-all-the-icons
  :ensure t
  :after treemacs
  :config
  (treemacs-load-theme "all-the-icons"))
(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)
(use-package lsp-treemacs
  :ensure t
  :after (treemacs lsp-mode)
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (lsp-treemacs-sync-mode))

;;; packages.init.el


;;; use-package
(unless package-archive-contents
  (package-refresh-contents))
(setq package-native-compile t)
(require 'package)
(use-package diminish
  :ensure t
  ;; diminish whatever isn't a package
  :config
  (diminish 'eldoc-mode))


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
(use-package linum-relative
  :ensure t
  :diminish linum-relative-mode
  :defer t
  :hook (prog-mode . linum-relative-mode)
  :init
  (setq linum-relative-backend 'display-line-numbers-mode))

;; magit
(use-package magit
  :ensure t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (with-eval-after-load 'magit-mode
	(add-hook 'after-save-hook 'magit-after-save-refresh-status t)))
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
  (evil-set-initial-state 'pdf-view-mode 'emacs)
  (evil-set-initial-state 'org-agenda-mode 'emacs)
  (evil-set-initial-state 'dashboard-mode 'emacs)

  (evil-set-leader 'normal (kbd ";"))
  (evil-mode))
;;(use-package evil-commentary
;;  :ensure t
;;  :after evil
;;  :config
;;  (add-hook 'prog-mode-hook 'evil-commentary-mode))
(use-package evil-collection
  :ensure t
  :diminish evil-collection-unimpaired-mode
  :after evil
  :config
  (evil-collection-init))
(use-package evil-snipe
  :ensure t
  :diminish evil-snipe-local-mode
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
;;(use-package base16-theme
;;  :ensure t
;;  :defer t)
(use-package cyberpunk-theme
  :ensure t)
(use-package monokai-pro-theme
  :ensure t
  :defer t)
(use-package kaolin-themes
  :ensure t
  :defer t)
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))
(use-package mini-modeline
  :ensure t
  :diminish mini-modeline-mode)
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
        dashboard-set-footer nil
        dashboard-set-heading-icons t
        dashboard-center-content t
        dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name
        dashboard-items '((projects . 10)
                          )
        )
  ;; agenda specifics
  (add-to-list 'dashboard-items '(agenda) t)
  (setq dashboard-week-agenda t
        dashboard-filter-agenda-entry 'dashboard-no-filter-agenda))

;; ivy
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t
		enable-recursive-minbuffers t
		ivy-re-builders-alist '((t . ivy--regex-fuzzy))
		)
  :config
  (ivy-mode))
(use-package flx
  :ensure t)
(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-rich-path-style 'abbrev))
(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))
(use-package counsel
  :ensure t
  :diminish counsel-mode
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

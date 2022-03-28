;;; packages.init.el


;;; use-package
(package-initialize)
(setq package-check-signature nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless package-archive-contents
  (package-refresh-contents))
(eval-when-compile
  (dolist (package '(use-package))
	(unless (package-installed-p package)
	  (package-install package))
	(require package)))


;;; general
(use-package ace-jump-mode
  :ensure t
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode))
(use-package auto-sudoedit
  :ensure t
  :config
  (auto-sudoedit-mode 1))
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-set-leader 'normal (kbd ","))
  (evil-mode))
;;(use-package evil-commentary
;;  :ensure t
;;  :after evil
;;  :config
;;  (add-hook 'prog-mode-hook 'evil-commentary-mode))
(use-package evil-god-state
  :ensure t
  :after evil
  :config
  (evil-define-key 'normal 'global (kbd "<leader>g") 'evil-execute-in-god-state)
  (evil-define-key 'god global-map [escape] 'evil-god-state-bail))
(use-package linum-relative
  :ensure t
  :config
  (setq linum-relative-backend 'display-line-numbers-mode)
  (add-hook 'company-mode-hook 'linum-relative-mode))
(use-package magit
  :ensure t
  :defer 2
  :config
  (with-eval-after-load 'magit-mode
	  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))
  ;; https://github.com/magit/magit/issues/2541
  (setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer (if (and (derived-mode-p 'magit-mode)
                         (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-diff-mode
                                 magit-stash-mode
                                 magit-status-mode)))
                    nil
                  '(display-buffer-same-window))))))
(use-package magit-todos
  :ensure t
  :after magit
  :init
  (setq magit-todos-ignored-keywords '("DONE"))
  (setq magit-todos t)
  :config
  (magit-todos-mode))
(use-package ivy
  :ensure t
  :config
  (setq ivy-count-format "%d/%d "
		ivy-use-virtual-buffers t
		enable-recursive-minibuffers t)
  (ivy-mode)
  (global-set-key (kbd "C-s") 'swiper))
(use-package vterm
  :ensure t)
(use-package multi-vterm
  :ensure t
  :after vterm)


;;; appearance
(use-package base16-theme
  :ensure t
  :defer t)
(use-package cyberpunk-theme
  :ensure t
  :defer t)
(use-package monokai-pro-theme
  :ensure t
  :defer t)
(use-package mini-modeline
  :ensure t
  :defer 1
  :config
  (mini-modeline-mode t))
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))
(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))


;;; lsp
(use-package lsp-mode
  :ensure t
  :init
  (add-hook 'python-mode-hook 'lsp)
  :config
  (setq lsp-modeline-diagnostics-enable t
		lsp-pyls-disable-warning t
		lsp-pyls-plugins-pycodestyle-enabled nil))
(use-package company
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  (setq company-minimum-prefix-length 1
		company-idle-delay 0.0
		company-show-numbers t))
(use-package flycheck
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)
(add-hook 'prog-mode-hook 'yas-minor-mode)


;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (global-set-key (kbd "C-c t") 'treemacs)
  :config
  (setq-default treemacs-use-follow-mode t
				treemacs-use-filewatch-mode t
				treemacs-use-git-mode 'deferred)
  (require 'treemacs-all-the-icons)
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


;;; language-specific
(use-package yaml-mode
  :ensure t
  :defer t)


;;; writing
(use-package fountain-mode
  :ensure t
  :defer t)
(use-package writeroom-mode
  :ensure t
  :defer t)
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :commands (markdown-mode gfm-mode)
  :config
  (setq markdown-command "pandoc"))
(require 'auth-source)
(use-package grip-mode
  :ensure t
  :after markdown-mode
  :config
  (setq grip-binary-path "/usr/bin/grip")
  (setq grip-url-browser "qutebrowser")
  (let ((credential (auth-source-user-and-password "api.github.com")))
	(setq grip-github-user (car credential)
		  grip-github-password (cadr credential)))
  (setq grip-preview-use-webkit nil)
  ;; C-c C-c g
  (define-key markdown-mode-command-map (kbd "g") #'grip-mode))


;;; exwm-specific
(when *exwm-enabled*
  (use-package exwm
	:ensure t
	:config
	(require 'exwm-config))
  (use-package counsel
	:ensure t)
  (use-package desktop-environment
	:ensure t
	:after exwm
	(desktop-environment-mode)))

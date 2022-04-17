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
(setq package-native-compile t)


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
(use-package linum-relative
  :ensure t
  :config
  (setq linum-relative-backend 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'linum-relative-mode))
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
  (setq magit-todos-ignored-keywords '(""))
  :config
  (magit-todos-mode))
(use-package which-key
  :ensure t
  :init
  (setq which-key-show-early-on-C-h t
	which-key-seconday-delay 0.05
	which-key-idle-delay 1.5)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))
(use-package tree-sitter
  :ensure t)
(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;;; evil-mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :config
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
(use-package evil-god-state
  :ensure t
  :after evil
  :config
  (evil-define-key 'normal 'global (kbd ",") 'evil-execute-in-god-state)
  (evil-define-key 'god global-map [escape] 'evil-god-state-bail))
(use-package undo-fu
  :ensure t
  :after evil
  :config
  (evil-define-key 'normal 'global "R" 'evil-redo)
  (evil-define-key 'normal 'global "r" 'evil-replace-state))


;;; appearance
(when *theme-magic-enabled*
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
		'((feebleline-line-number		:post "" :fmt "%4s")
		  (feebleline-column-number		:pre ":" :fmt "%-2s")
		  (feebleline-file-directory		:face feebleline-dir-face :post "")
		  (feebleline-file-or-buffer-name	:face font-lock-warning-face :post "")
		  (feebleline-file-modified-star	:face font-lock-warning-face :post "")
		  (magit-get-current-branch		:face feebleline-git-face :pre " -> ")
		  ))
  (feebleline-mode 1))
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))
(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))


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
  :after ivy
  :config
  (global-set-key "\C-s" 'swiper))


;;; lsp
(use-package lsp-ui
  :ensure t
  :after lsp-mode)
(use-package lsp-mode
  :ensure t
  :after (company flycheck which-key)
  :init
  (add-hook 'prog-mode-hook	'yas-minor-mode)
  (add-hook 'lsp-mode-hook	#'lsp-enable-which-key-integration)
  ;; language-specific
  (add-hook 'python-mode-hook 'lsp)
  (add-hook 'c-mode-hook 'lsp)
  :config
  ;; direct lsp config
  (setq lsp-lens-enable nil)

  ;; NOTE: not sure this does anything
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-prefer-flymake nil)
  
  ;; lsp related settings
  (setq lsp-pyls-disable-warning t
	lsp-pyls-plugins-pycodestyle-enabled nil
	lsp-ui-doc-enable t
	lsp-ui-sideline-show-diagnostics t
	))
(use-package company
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  (setq company-minimum-prefix-length 1
	company-idle-delay 0.0
	company-show-numbers t
	company-transformers nil
	company-lsp-async t
	company-lsp-cache-candidates nil)
  :config
  (setq company-backends '((company-yasnippet company-dabbrev-code company-capf company-keywords company-files))))
(use-package company-box
  :ensure t
  :after company
  :hook (company-mode . company-box-mode))
(use-package flycheck
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))


;; treemacs
(use-package treemacs
  :ensure t
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
(use-package yaml-mode			;; yaml
  :ensure t
  :defer t)

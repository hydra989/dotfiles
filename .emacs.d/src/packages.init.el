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
(use-package avy
  :ensure t
  :after evil
  :config
  (evil-define-key 'normal 'global (kbd "SPC") 'avy-goto-char))
(use-package dtrt-indent
  :ensure t
  :hook ((prog-mode emacs-lisp-mode) . dtrt-indent-mode))
(use-package ibuffer-vc
  :ensure t
  :config
  (add-hook 'ibuffer-hook
			(lambda ()
			  (ibuffer-vc-set-filter-groups-by-vc-root)
			  (unless (eq ibuffer-sorting-mode 'major-mode)
				(ibuffer-do-sort-by-major-mode)))))
(use-package linum-relative
  :ensure t
  :defer t
  :hook (company-mode . linum-relative-mode)
  :init
  (setq linum-relative-backend 'display-line-numbers-mode))
(use-package magit
  :ensure t
  :defer 2
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
  :init
  (setq magit-todos-ignored-keywords '(""))
  :config
  (magit-todos-mode))
(use-package tree-sitter
  :ensure t
  :defer t
  :hook (prog-mode . tree-sitter-mode))
(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))

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
(use-package evil-snipe
  :ensure t
  :after evil
  :config
  (evil-define-key 'visual evil-snipe-local-mode-map "z" 'evil-snipe-s)
  (evil-define-key 'visual evil-snipe-local-mode-map "Z" 'evil-snipe-S))
(use-package undo-fu
  :ensure t
  :after evil
  :config
  (evil-define-key 'normal 'global "\C-r" 'evil-redo))

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
		'((feebleline-line-number		    :post "" :fmt "%4s")
		  (feebleline-column-number		    :pre ":" :fmt "%-2s")
		  (feebleline-file-directory		:face feebleline-dir-face :post "")
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
  :init
  (setq
   dashboard-center-content t
   dashboard-set-footer nil
   ; icons
   dashboard-set-heading-icons nil
   dashboard-set-file-icons t
   )
  (setq dashboard-items '((recents . 5)
                          ))
  :config
  (dashboard-setup-startup-hook))


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
  :after lsp-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse t
        lsp-ui-doc-delay 0.5
        lsp-ui-peek-enable t
		lsp-ui-sideline-show-diagnostics t
        ))
(use-package lsp-mode
  :ensure t
  :defer t
  :hook ((c-mode python-mode) . lsp)
  :init
  
  (add-hook 'prog-mode-hook	'yas-minor-mode)
  (add-hook 'lsp-mode-hook	#'lsp-enable-which-key-integration)
  :config
  (yas-reload-all)
  
  ;; direct lsp config
  (setq lsp-lens-enable nil)

  ;; NOTE: not sure this does anything
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-prefer-flymake nil)
  
  ;; lsp related settings
  (setq lsp-pyls-disable-warning t
		lsp-pyls-plugins-pycodestyle-enabled nil
		))
(use-package company
  :ensure t
  :defer t
  :hook (prog-mode . company-mode)
  :init
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
  :defer t
  :hook (company-mode . company-box-mode))
(use-package company-quickhelp
  :ensure t
  :defer t
  :hook (company-mode . company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay 0.4))
(use-package flycheck
  :ensure t
  :defer t
  :hook (prog-mode . flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
(use-package yasnippet
  :ensure t
  :defer t)


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
  (treemacs-load-theme "all-the-icons"))
(use-package treemacs-all-the-icons
  :ensure t
  :defer t)
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
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))
(use-package dockerfile-mode    ;; dockerfiles
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  (put 'dockerfile-image-name 'safe-local-variable #'stringp))
(use-package nix-mode			;; nix
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode)))
(use-package go-mode            ;; go
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-hook 'go-mode #'lsp-mode-deferred))

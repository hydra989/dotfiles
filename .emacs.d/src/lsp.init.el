;;; lsp.init.el
;;; configuration specific to programming/lsp


(use-package dtrt-indent
  :ensure t
  :defer t
  :hook ((prog-mode emacs-lisp-mode) . dtrt-indent-mode))
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package tree-sitter
  :ensure t
  :defer t
  :hook ((lsp-mode elpy-mode) . tree-sitter-mode))
(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))

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
  :hook ((c-mode) . lsp)
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
(use-package lua-mode           ;; lua
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode)))
(use-package elpy               ;; python
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

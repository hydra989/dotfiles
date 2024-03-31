(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(winner-mode 1)
(pixel-scroll-mode 1)
(fringe-mode '(1 . 1))      ;; "minimal" preset

(setq
 ring-bell-function 'ignore ;; loadon't make noise
 vc-follow-symlinks t
 load-prefer-newer t        ;; prefer newer .elc...
 delete-old-versions t      ;; ..and delete old ones
 use-short-answers t        ;; y/n instead of yes/no
 make-backup-files nil
 create-lockfiles nil
 native-comp-deferred-compilation t
 comp-async-report-warnings-errors nil
 scroll-preserve-screen-position 1
 scroll-conservatively 10000
 )

(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq backward-delete-char-untabify-method 'hungry)

(custom-set-variables
 '(org-directory "~/s/org")
 '(org-agenda-files (list "~/s/org/agenda")))

(setq display-line-numbers 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(defun emacsclient-gui-setup (frame)
  (select-frame frame)
  (load-theme 'oxocarbon)
  ;; transparency 
  (set-frame-parameter (selected-frame) 'alpha '(90 . 70))
  (add-to-list 'default-frame-alist '(alpha . (90 . 70)))

  (remove-hook 'after-make-frame-functions #'emacsclient-gui-setup)
  )

(add-hook 'after-make-frame-functions #'emacsclient-gui-setup)

(defun config-visit()
  (interactive)
  (find-file "/home/hydra/s/dotfiles/emacs.d.org"))

(defun full-auto-save ()
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (buffer-file-name) (buffer-modified-p))
          (basic-save-buffer)))))

(put 'kill-process 'interactive-form
     '(interactive
       (let ((proc (get-buffer-process (current-buffer))))
	 (if (process-live-p proc)
	     (unless (yes-or-no-p (format "Kill %$ " proc))
	       (error "Process not killed"))
	   (error (format "Buffer %s has no process" (buffer-name))))
	 nil)))

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(setq custom-safe-themes t)

(add-to-list 'custom-theme-load-path "/home/hydra/.emacs.d/themes")

(use-package autothemer
  :ensure t)
(use-package catppuccin-theme
  :init
  (setq catpuccin-flavor 'frappe))
(use-package cyberpunk-theme
  :defer t)
(use-package nordic-night-theme
  :defer t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package magit
  :ensure t)

(use-package evil
  :ensure t
  :init
  ;; evil-collection wants this set
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package helm
  :ensure t
  :config
  (require 'helm-autoloads)
  (helm-mode 1)

  ;; bindings
  (global-set-key (kbd "M-x") 'helm-M-x)
  )

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("C-' t" . treemacs)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(setq
 org-hide-emphasis-markers t ;; hide emphasis markup (*bold*, etc)
 org-startup-with-inline-images t
 org-cycle-separator-lines -1
 org-hide-leading-stars t
 )

(add-hook 'org-mode-hook 'visual-line-mode)

(add-hook 'org-mode-hook 'org-indent-mode)

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyung/tree-sitter-markdown")
        (nix "https://github.com/nix-community/tree-sitter-nix")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        )
      )

(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (css-mode . css-ts-mode)
        (elisp-mode . elisp-ts-mode)
        (go-mode . go-ts-mode)
        (html-mode . html-ts-mode)
        (java-mode . java-ts-mode)
        (make-mode . make-ts-mode)
        (markdown-mode . markdown-ts-mode)
        (nix-mode . nix-ts-mode)
        (python-mode . python-ts-mode)
        ))

(use-package nix-mode
  :mode "\\.nix'")
(use-package nix-ts-mode)

(use-package eglot
  :ensure t
  :defer t
  :hook ((java-mode . eglot-ensure)
         (nix-mode . eglot-ensure)
         (lua-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         )
  )

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)
(global-set-key (kbd "C-' c") 'config-visit)
(global-set-key (kbd "C-' s") 'full-auto-save)
(global-set-key (kbd "C-' t") 'toggle-transparency)

(defvar *hostname*            (getenv "HOSTNAME"))

(scroll-bar-mode -1)    ;; disable scroll bar
(tool-bar-mode -1)      ;; disable tool bar
(menu-bar-mode -1)      ;; disable menu bar
(winner-mode 1)         ;; C-c {left, right} for window manipulation
(pixel-scroll-mode 1)   ;; smooth scrolling
;; !? -- dragons
(fringe-mode 1)
(global-auto-revert-mode 1)

;;(add-hook 'prog-mode-hook 'indent-tabs-mode)

(setq
   ring-bell-function 'ignore                    ;; don't make noise
   vc-follow-symlinks t                          ;; set with intention of
;;                                                  playing nice with hardlinks
   load-prefer-newer t                           ;; prefer newer .elc...
   delete-old-versions t                         ;; ...and delete the old editions
   use-short-answers t                           ;; y/n instead of yes/no
   backup-by-copying t                           ;; don't delink hardlinks (?)
   c-default-style "linux"                       ;; set c style to something tolerable

   ;; don't clutter directories with backups
   make-backup-files nil
   create-lockfiles nil

   ;; frame resize settings
   frame-inhibit-implied-resize t                ;; shouldn't wildly resize during startup
   frame-resize-pixelwise t

   ;; elisp compiliation settings
   native-comp-deferred-compilation t
   comp-async-report-warnings-errors nil

   ;; dired
   dired-kill-when-opening-new-dired-buffer t    ;; don't allow dired to clutter up C-x b and soforth

   ;; !? -- dragons, relics of time past
   use-dialog-box nil
   echo-keystrokes 0.2
   )

(set-frame-parameter (selected-frame) 'alpha '(95 . 90))
(add-to-list 'default-frame-alist '(alpha . (95 . 90)))

(custom-set-variables
 '(org-directory "~/s/org")
 '(org-agenda-files (list "~/s/org/agenda")))

(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

(defun daemon-gui-setup (frame)
  (select-frame frame)
  (load-theme 'cyberpunk t)
  (set-frame-font "Hack 10" nil t)
  (mini-modeline-mode t)
  ;;(awesome-tray-mode 1)
  (remove-hook 'after-make-frame-functions #'daemon-gui-setup)
  )

;; 1-6-22: not sure how this works when emacs itself is opened (not emacsclient)
(add-hook 'after-make-frame-functions #'daemon-gui-setup)

(defun config-visit ()
  (interactive)
  (find-file "/home/hydra/s/dotfiles/.emacs.d/config.org"))

(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))
  ;;(insert initial-scratch-message))

(defun full-auto-save ()
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (buffer-file-name) (buffer-modified-p))
          (basic-save-buffer)))))

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

(put 'kill-process 'interactive-form
	 '(interactive
	       (let ((proc (get-buffer-process (current-buffer))))
		 (if (process-live-p proc)
			 (unless (yes-or-no-p (format "Kill %S? " proc))
			       (error "Process not killed"))
		       (error (format "Buffer %s has no process" (buffer-name))))
		 nil)))

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)
(global-set-key (kbd "C-' c") 'config-visit)
(global-set-key (kbd "C-' b") 'create-scratch-buffer)
(global-set-key (kbd "C-' s") 'full-auto-save)
(global-set-key (kbd "C-' t") 'vterm)
(global-set-key (kbd "C-' a") 'org-agenda)

(when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings))

(use-package diminish
  ;; diminish whatever isn't a package
  :config
  (diminish 'eldoc-mode)
)

(use-package avy
  :after evil
  :config
  (evil-define-key 'normal 'global (kbd "SPC") 'avy-goto-char))

(use-package bufler
  :init
  ;; default/example config
  (bufler-defgroups
   (group
    (auto-workspace))
   (group
    (group-or "*Help/Info*"
              (mode-match "*Help*" (rx bos "help-"))
              (mode-match "*Info*" (rx bos "info-"))))
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
    (auto-mode))
   (dir user-emacs-directory)
   (group
    ; Subgroup collecting buffers in `org-directory' (or "~/org" if
    ; `org-directory' is not yet defined).
    (dir (if (bound-and-true-p org-directory)
             org-directory
           "~/org"))
    (group
      (auto-indirect)
      (auto-file))
    (group-not "*special*" (auto-file))
    (auto-mode))
   (group
    (auto-project))
   (auto-directory)
   (auto-mode))
   :config
   (global-set-key (kbd "C-x C-b") 'bufler))

(use-package linum-relative
  :diminish linum-relative-mode
  :defer t
  :hook (prog-mode . linum-relative-mode)
  :init
  (setq linum-relative-backend 'display-line-numbers-mode))

(use-package magit
  :config
  ;; https://github.com/magit/magit/issues/2541#issuecomment-180611059
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
                    '(display-buffer-same-window)))))

  (with-eval-after-load 'magit-mode
    (add-hook 'after-save-hook 'magit-after-save-refresh-status t)))

(use-package magit-todos
  :after magit
  :config
  (setq magit-todos-ignored-keywords '(""))
  (magit-todos-mode))

(use-package evil
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
(use-package evil-commentary
  :after evil
  :config
  (add-hook 'prog-mode-hook 'evil-commentary-mode))
(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :after evil
  :config
  (evil-collection-init))
(use-package evil-snipe
  :diminish evil-snipe-local-mode
  :after evil
  :config
  (evil-snipe-mode +1))
(use-package undo-fu
  :after evil
  :config
  (evil-define-key 'normal 'global "\C-r" 'evil-redo))
(use-package evil-mc
  :after evil
  :config
  (evil-define-key 'visual evil-mc-key-map
    "A" #'evil-mc-make-cursor-in-visual-selection-end
    "I" #'evil-mc-make-cursor-in-visual-selection-beg)
  (global-evil-mc-mode 1))

(use-package theme-magic
  :config
  (theme-magic-export-theme-mode))

;;(use-package base16-theme
;;  :defer t)
(use-package cyberpunk-theme)
(use-package monokai-pro-theme
  :defer t)
(use-package kaolin-themes
  :defer t)
(use-package ef-themes
  :defer t)

;; icons
(use-package all-the-icons
  :if (display-graphic-p))

(use-package mini-modeline
  :diminish mini-modeline-mode
)

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (add-to-list 'dashboard-items '(agenda) t)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :custom (
           (inhibit-start-screen t)
           ;(inital-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
           (dashboard-set-init-info nil)
           (dashboard-set-footer nil)
           (dashboard-set-heading-icons t)
           (dashboard-center-content t)
           (dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
           (dashboard-items '(
                              (recents . 5)
                              (projects . 10)
                              ))
           (dashboard-week-agenda t)
           ))

(use-package ivy
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t
        enable-recursive-minbuffers t
        ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        )
  :config
  (ivy-mode))
(use-package flx)
(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-rich-path-style 'abbrev))
(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))
(use-package counsel
  :diminish counsel-mode
  :after ivy
  :config
  (counsel-mode))
(use-package swiper
  :defer t
  :after ivy
  :config
  (global-set-key "\C-s" 'swiper))

(use-package projectile
  ;:diminish (projectile-mode . "Proj.")
)
(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode))

(use-package treemacs
  :diminish treemacs-mode
  :config
  (global-set-key (kbd "C-c t") 'treemacs)
  (setq-default treemacs-use-follow-mode nil
                treemacs-use-filewatch-mode t
                treemacs-use-git-mode 'deferred))
;; (use-package treemacs-all-the-icons
;;   :after treemacs
;;   :config
;;   (treemacs-load-theme "all-the-icons"))
(treemacs-load-all-the-icons-with-workaround-font "Hack")
(use-package treemacs-evil
  :after (treemacs evil))
(use-package treemacs-magit
  :after (treemacs magit))
(use-package lsp-treemacs
  :after (treemacs lsp-mode)
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (lsp-treemacs-sync-mode))

(use-package which-key
  :config
  (setq which-key-show-early-on-C-h t
        which-key-popup-type 'frame
        )
)

(use-package tree-sitter
  :config
  (global-tree-sitter-mode))
(use-package tree-sitter-langs
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package yasnippet
  ;:diminish yas-minor-mode
  )

(use-package company
  :hook (prog-mode . company-mode)
  :init
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0
        company-show-numbers t
        company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil
        company-backends '((
                            company-yasnippet
                            company-dabbrev-code
                            company-capf
                            company-keywords
                            company-files
                            ))
        ))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay 0.4))

(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package lsp-mode
  :after yasnippet
  :defer t
  :init
  ;; hooks for individual languages
  (add-hook 'c-mode-hook 'lsp)
  ;; hooks for other modes thait tie into lsp-mode
  (add-hook 'prog-mode-hook	'yas-minor-mode)
  :config

  ;; yasnippet loads prior to this
  (yas-reload-all)

  ;; direct lsp config
  (setq lsp-lens-enable nil
        lsp-diagnostics-provider :flycheck
        lsp-prefer-flymake nil))

(use-package lsp-ui
  :after lsp-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

  ;; lsp-ui-doc
  ;; NOTE: there's gotta be some redundancy here somewhere
  (lsp-ui-doc-mode)
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse t
        lsp-ui-doc-delay 0.5
        lsp-ui-peek-enable t
        lsp-ui-sideline-show-diagnostics t
        )
  )

(use-package dockerfile-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  (put 'dockerfile-image-name 'safe-local-variable #'stringp))

(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package go-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-hook 'go-mode-hook #'lsp))

(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp))

(use-package lua-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode)))

(use-package lsp-nix
  :ensure lsp-mode
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-nix-nil-formatter ["nixpkgs-fmt"]))

(use-package nix-mode
  :hook (nix-mode . lsp)
  :init
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode)))

(use-package yaml-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(setq
  org-edit-src-content-indentation 2
  org-hide-emphasis-markers t         ;; hide * and whatnot
  org-src-tab-acts-natively t
  org-startup-indented t
  line-spacing 3
)

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package toc-org
  :defer t
  :init
  (add-hook 'org-mode-hook 'toc-org-mode))

(use-package org-superstar
  :defer t
  :init
  (add-hook 'org-mode-hook 'org-superstar-mode))

(use-package fountain-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.fountain\\'" . fountain-mode)))

(use-package writeroom-mode)

(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

(setq custom-file "/home/hydra/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

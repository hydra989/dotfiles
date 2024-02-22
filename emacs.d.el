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
 )

(custom-set-variables
 '(org-directory "~/s/org")
 '(org-agenda-files (list "~/s/org/agenda")))

(defun emacsclient-gui-setup (frame)
  (select-frame frame)
  (load-theme 'wintermute t)
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

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'custom-theme-load-path "/home/hydra/.emacs.d/themes")

(use-package autothemer
  :ensure t)
(use-package cyberpunk-theme
  :defer t)
(use-package nordic-night-theme
  :defer t)

(use-package nix-mode
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
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (make "https://github.com/alemuller/tree-sitter-make")
        (nix "https://github.com/nix-community/tree-sitter-nix")
        (markdown "https://github.com/ikatyung/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        )
      )

(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (css-mode . css-ts-mode)
        (elisp-mode . elisp-ts-mode)
        (go-mode . go-ts-mode)
        (html-mode . html-ts-mode)
        (java-mode . java-ts-mode)
        (make-mode . make-ts-mode)
        (nix-mode . nix-ts-mode)
        (markdown-mode . markdown-ts-mode)
        (python-mode . python-ts-mode)
        ))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)
(global-set-key (kbd "C-' c") 'config-visit)
(global-set-key (kbd "C-' s") 'full-auto-save)

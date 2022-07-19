;;; org-anno.init.el


;;; general
(use-package annotate           ;; replacement for google docs suggestions
  :ensure t
  :defer t
  :hook (writeroom-mode . annotate-mode))

;;; org-mode
(use-package org-noter			;; annotating with docview
  :ensure t
  :defer t)

;;; writing modes 
(use-package fountain-mode		;; screenplays
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.fountain\\'" . fountain-mode))
  (defun export-to-pdf ()
    (shell-command-to-string (format "afterwriting --config afterwriting-config.json --source %s --pdf --overwrite" buffer-file-name)))
  :config
  (add-hook 'after-save-hook #'export-to-pdf))
(use-package writeroom-mode		;; distraction free writing
  :ensure t
  :defer t)
(use-package markdown-mode		;; markdown
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
  :config
  (setq markdown-command "pandoc"))

;;; org-anno.init.el


;;; general
(use-package annotate           ;; replacement for google docs suggestions
  :ensure t
  :defer t
  :hook (writeroom-mode . annotate-mode))
(use-package pdf-tools
  :ensure t
  :init
  (pdf-tools-install))

;;; org-mode
(use-package org-noter			;; annotating with docview
  :ensure t
  :defer t)

;;; writing modes 
(use-package fountain-mode		;; screenplays
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.fountain\\'" . fountain-mode))
  (defun add-fountain-hook ()
    (add-hook 'after-save-hook #'export-to-pdf))
  (add-hook 'fountain-mode #'add-fountain-hook)
  (add-hook 'fountain-mode #'writeroom-mode))
(use-package writeroom-mode		;; distraction free writing
  :ensure t)
(use-package markdown-mode		;; markdown
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
  :config
  (setq markdown-command "pandoc"))

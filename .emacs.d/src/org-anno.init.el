;;; org-anno.init.el


;;; org-mode
(use-package org-noter			;; annotating with docview
  :ensure t)


;;; writing modes 
(use-package fountain-mode		;; screenplays
  :ensure t
  :defer t)
(use-package writeroom-mode		;; make writing prettier
  :ensure t
  :defer t)
(use-package markdown-mode		;; markdown
  :ensure t
  :defer t
  :mode ("\\.md\\'" . gfm-mode)
  :commands (markdown-mode gfm-mode)
  :config
  (setq markdown-command "pandoc"))

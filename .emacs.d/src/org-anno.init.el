;;; org-anno.init.el


;;; writing modes 
(use-package fountain-mode		;; screenplays
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.fountain\\'" . fountain-mode)))
(use-package writeroom-mode		;; distraction free writing
  :ensure t)
(use-package markdown-mode		;; markdown
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

;;; general
;(use-package pdf-tools
;  :ensure t
;  :config
;  (add-to-alist 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
;  (setq-default pdf-view-display-size 'fit-page)
;; uncomment upon emacs initial install
;  (pdf-tools-install))
;  )

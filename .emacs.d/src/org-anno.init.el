;;; org-anno.init.el


;;; writing modes 
(use-package fountain-mode		;; screenplays
  :init
  (add-to-list 'auto-mode-alist '("\\.fountain\\'" . fountain-mode)))
(use-package writeroom-mode)		;; distraction free writing
(use-package markdown-mode		;; markdown
  :defer t
  :commands (markdown-mode gfm-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

;;; general
;(use-package pdf-tools
;  :config
;  (add-to-alist 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
;  (setq-default pdf-view-display-size 'fit-page)
;; uncomment upon emacs initial install
;  (pdf-tools-install))
;  )

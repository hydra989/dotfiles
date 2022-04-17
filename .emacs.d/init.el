;;; init.el
;;;
;;;		TODO: unify style throughout
;;;		TODO: latex previews?
;;;		TODO: fallback font for hack?


;; performance things
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(defvar *theme-magic-enabled* t)	;; true for pywal environments
(defvar *transparency* nil)

;; https://www.emacswiki.org/emacs/DotEmacsModular
(defconst emacs-config-directory "~/.emacs.d/" "")
(defun load-config-file (filelist)
  (dolist (file filelist)
    (load (expand-file-name 
           (concat emacs-config-directory file)))
     ;;(message "Loaded config file:%s" file)
     ))
(load-config-file '(
		    "src/defaults.init"
		    "src/func.init"
		    "src/packages.init"
		    "src/org-anno.init"
		    ))

;; transparancy
(when *transparency*
  (set-frame-parameter (selected-frame) 'alpha '(85 . 75))
  (add-to-list 'default-frame-alist '(alpha . (85 . 75))))

(load-theme 'monokai-pro t)
(set-frame-font "Hack-11.5" nil t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fb83a50c80de36f23aea5919e50e1bccd565ca5bb646af95729dc8c5f926cbf3" "e3a1b1fb50e3908e80514de38acbac74be2eb2777fc896e44b54ce44308e5330" "b6269b0356ed8d9ed55b0dcea10b4e13227b89fd2af4452eee19ac88297b0f99" "c8b83e7692e77f3e2e46c08177b673da6e41b307805cd1982da9e2ea2e90e6d7" "24168c7e083ca0bbc87c68d3139ef39f072488703dcdd82343b8cab71c0f62a7" "b8929cff63ffc759e436b0f0575d15a8ad7658932f4b2c99415f3dde09b32e97" "bc4c89a7b91cfbd3e28b2a8e9e6750079a985237b960384f158515d32c7f0490" "7575474658c34b905bcec30a725653b2138c2f2d3deef0587e3abfae08c5b276" "ed49a2f0e2f329308a17a0d9ecdbe7dc05d440554f0e7bfd1497778f8ffde877" "8c1dd3d6fdfb2bee6b8f05d13d167f200befe1712d0abfdc47bb6d3b706c3434" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" default))
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(package-selected-packages
   '(theme-magic company-box treemacs-all-the-icons cyberpunk-theme treemacs yaml-mode writeroom-mode use-package undo-fu treemacs-magit treemacs-evil pretty-hydra multiple-cursors multi-vterm monokai-pro-theme mini-modeline map lsp-jedi linum-relative impatient-mode grip-mode fountain-mode flycheck exwm evil-commentary counsel cfrs ccls base16-theme avy auto-sudoedit all-the-icons))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
	 (40 . "#CC9393")
	 (60 . "#DFAF8F")
	 (80 . "#D0BF8F")
	 (100 . "#E0CF9F")
	 (120 . "#F0DFAF")
	 (140 . "#5F7F5F")
	 (160 . "#7F9F7F")
	 (180 . "#8FB28F")
	 (200 . "#9FC59F")
	 (220 . "#AFD8AF")
	 (240 . "#BFEBBF")
	 (260 . "#93E0E3")
	 (280 . "#6CA0A3")
	 (300 . "#7CB8BB")
	 (320 . "#8CD0D3")
	 (340 . "#94BFF3")
	 (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

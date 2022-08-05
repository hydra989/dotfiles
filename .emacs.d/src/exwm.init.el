;;; exwm.init.el

(use-package exwm
  :ensure t
  :config
  (require 'exwm-config))
(use-package desktop-environment
  :ensure t
  :after exwm
  :config
  (desktop-environment-mode))


;;; functions

(defun run_qutebrowser ()
  "run qutebrowser"
  (interactive)
  (start-process-shell-command "qutebrowser" nil "qutebrowser"))
(defun run_steam ()
  "start steam"
  (interactive)
  (start-process-shell-command "Steam" nil "steam"))
(defun run_spotify ()
  "run spotify"
  (interactive)
  (start-process-shell-command "Spotify" nil "flatpak run com.spotify.Client"))

(defun exwm-multimonitor ()
  (interactive)
  (require 'exwm-randr)
  ;; (require 'exwm-systemtray)
  (exwm-randr-enable)
  ;(start-process-shell-command "xrandr" nil "xrandr --output DVI-D-0 --off --output DP-0 --off --output DP-1 --mode 1920x1080 --pos 2560x0 --rotate normal --output HDMI-0 --primary --mode 2560x1080 --pos 0x91 --rotate normal --output DP-2 --off --output DP-3 --off --output DP-4 --off --output DP-5 --off")
  ;(setq exwm-randr-workspace-monitor-plist '(3 "DP-1" 4 "DP-1" 5 "DP-1"))
  )

;; tedroden@github
(defun goto-wm-next-workspace ()
  "go to next workspace if we're under the limit"
  (interactive)
  (let ((num (+ 1 exwm-workspace-current-index)))
	(if (< num exwm-workspace-switch-create-limit)
	    (exwm-workspace-switch-create num)
	  (message "Too many workspaces"))))
(defun goto-wm-prev-workspace ()
  "go to previous workspace if we're not above the limit"
  (interactive)
  (if (> exwm-workspace-current-index 0)
	  (exwm-workspace-switch-create (- exwm-workspace-current-index 1))
	(message "Already on first workspace")))


;;; configuration

(exwm-config-ido)
;;(exwm-multimonitor)
(exwm-workspace-switch-create 1)
(setq
      ;; workspaces 0-5
      exwm-workspace-number 6
	  ;; keybinds
	  exwm-input-global-keys
	  `(
		,@(mapcar (lambda (i)
					`(,(kbd (format "s-%d" i)) .
					  (lambda ()
						(interactive)
						(exwm-workspace-switch-create ,i))))
				  (number-sequence 0 9))
		([?\s-k] . kill-process)
		([?\s-r] . exwm-reset)))

;; C-q to send next keypress directly
(bind-key "C-q"
		  (lambda ()
			(interactive)
			(exwm-input-send-next-key 1))
		  exwm-mode-map)

(exwm-input-set-key (kbd "C-' s") 'exwm-workspace-switch)
(exwm-input-set-key (kbd "C-' m") 'exwm-workspace-move-window)
(exwm-input-set-key (kbd "C-' >") 'goto-wm-next-workspace)
(exwm-input-set-key (kbd "C-' <") 'goto-wm-prev-workspace)

(exwm-input-set-key (kbd "C-' r") 'counsel-linux-app)
(exwm-input-set-key (kbd "<home>") 'counsel-linux-app)

(exwm-input-set-key (kbd "C-' Q") 'run_qutebrowser)
(exwm-input-set-key (kbd "C-' G") 'run_steam)
(exwm-input-set-key (kbd "C-' S") 'run_spotify)

(exwm-input-set-key (kbd "C-' t") 'multi-vterm)

;; stolen from exwm wiki
;; update exwm buffer titles
(add-hook 'exwm-update-class-hook
          (lambda ()
			(unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
						(string= "gimp" exwm-instance-name))
			  (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
			(when (or (not exwm-instance-name)
					  (string-prefix-p "sun-awt-X11-" exwm-instance-name)
					  (string= "gimp" exwm-instance-name))
			  (exwm-workspace-rename-buffer exwm-title))))

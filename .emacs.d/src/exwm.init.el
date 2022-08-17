;;; exwm.init.el


;;; functions

(defun exwm/run-firefox ()
  "run firefox"
  (interactive)
  (start-process-shell-command "Firefox" nil "firefox"))
(defun exwm/run-steam ()
  "start steam"
  (interactive)
  (start-process-shell-command "Steam" nil "steam"))
(defun exwm/run-spotify ()
  "run spotify"
  (interactive)
  (start-process-shell-command "Spotify" nil "flatpak run com.spotify.Client"))
(defun exwm/set-wallpaper-multimonitor ()
  "set wallpaper to ~/.background-image"
  (interactive)
  (start-process-shell-command "feh" nil "feh --bg-scale ~/.background-image ~/.background-image"))
(defun exwm/run-xbindkeys ()
  "run xbindkeys"
  (interactive)
  (start-process-shell-command "xbindkeys" nil "xbindkeys"))

(use-package desktop-environment
  :ensure t
  :after exwm
  :config
  (desktop-environment-mode))
(use-package exwm
  :ensure t
  :config
  (require 'exwm-config)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  (defun exwm-multimonitor ()
	(interactive)
	(require 'exwm-randr)
	;; (require 'exwm-systemtray)
	(exwm-randr-enable)

	; stolen from arandr. use alt de for non-standard monitor setups
	(start-process-shell-command "xrandr" nil "xrandr --output eDP-1 --primary --mode 1920x1080 --pos 2560x425 --rotate normal --output HDMI-1 --mode 2560x1080 --pos 0x0 --rotate normal")
	(exwm/set-wallpaper-multimonitor)
	(setq exwm-randr-workspace-monitor-plist '( 2 "HDMI-1"))
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

  (exwm-config-misc)
  (exwm-multimonitor)
  (exwm/run-xbindkeys)

  (setq
   ;; workspaces 0-5
   exwm-workspace-number 6
   ;; window focus
   mouse-autoselect-window t
   focus-follows-mouse t
   ;; keybinds
   exwm-input-global-keys
   `(
	 ([?\s-k] . kill-process)
	 ([?\s-r] . exwm-reset)
	 ([?\s-w] . exwm-workspace-switch)
	 ([?\s->] . goto-wm-next-workspace)
	 ([?\s-<] . goto-wm-prev-workspace)
	 ([?\s-m] . exwm-workspace-move-window)

	 ,@(mapcar (lambda (i)
				 `(,(kbd (format "s-%d" i)) .
				   (lambda ()
					 (interactive)
					 (exwm-workspace-switch-create ,i))))
			   (number-sequence 0 9))
	 )
   )

  (exwm-workspace-switch-create 1)

  ;; C-q to send next keypress directly
  (bind-key "C-q"
			(lambda ()
			  (interactive)
			  (exwm-input-send-next-key 1))
			exwm-mode-map)

  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name))))

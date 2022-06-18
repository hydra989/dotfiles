;;; func.init.el


;; stolen from uncledavesemacs
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

;; this isn't mine, but not sure where i found it
(defun config-visit ()
  (interactive)
  (find-file "/home/hydra/.emacs.d/init.el"))

;; stolen from emacswiki
(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))
  ;;(insert initial-scratch-message))

;; https://www.emacswiki.org/emacs/AutoSave#h5o-4
;; save all buffers
(defun full-auto-save ()
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (buffer-file-name) (buffer-modified-p))
          (basic-save-buffer)))))

;; enable M-x kill-process (to kill the current buffer's process).
(put 'kill-process 'interactive-form
     '(interactive
       (let ((proc (get-buffer-process (current-buffer))))
         (if (process-live-p proc)
             (unless (yes-or-no-p (format "Kill %S? " proc))
               (error "Process not killed"))
           (error (format "Buffer %s has no process" (buffer-name))))
         nil)))

;;; bindings
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)
(global-set-key (kbd "C-' c") 'config-visit)
(global-set-key (kbd "C-' b") 'create-scratch-buffer)
(global-set-key (kbd "C-' s") 'full-auto-save)

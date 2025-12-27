(require 'cloel)

(defvar cloel-agent-clj-project (expand-file-name "cloel_agent.clj" (file-name-directory load-file-name)))
(defvar cloel-agent--current-goal nil)

(cloel-register-app "agent" cloel-agent-clj-project)

(defun cloel-agent--get-buffer ()
  (get-buffer-create "*Cloel Agent Terminal*"))

(defun cloel-agent-update-status (status)
  (with-current-buffer (cloel-agent--get-buffer)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (forward-line 1)
        (delete-region (line-beginning-position) (line-end-position))
        (insert (format "status: %s" status))))))

(defun cloel-agent-append-log (msg &optional type)
  (with-current-buffer (cloel-agent--get-buffer)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (let ((prefix (cond ((string= type "ai") "🤖 [AI reply] ")
                            ((string= type "error") "❌ [error] ")
                            (t "ℹ️ [monitor] "))))
          (insert (format "\n[%s] %s%s" (format-time-string "%H:%M:%S") prefix msg))))
      (let ((window (get-buffer-window (current-buffer))))
        (when window (set-window-point window (point-max)))))))

(defun cloel-agent--setup-buffer ()
  (with-current-buffer (cloel-agent--get-buffer)
    (setq-local buffer-read-only nil)
    (erase-buffer)
    (insert "=== CLOEL AGENT Terminal ===\n")
    (insert "status: ready\n")
    (insert "---------------------------\n")
    (display-buffer (current-buffer))))

(defun cloel-agent-start-process-confirm (client-id)
  (message "agent process started: %s" client-id)
  (cloel-agent--setup-buffer)
  (cloel-agent-update-status "link passed")
  (cloel-agent-send-request))

(defun cloel-agent-send-request ()
  (when cloel-agent--current-goal
    (cloel-agent-call-async "agent-success" cloel-agent--current-goal)))

;;;###autoload
(defun cloel-agent-run (goal)
  (interactive "sAI Task Goal: ")
  (setq cloel-agent--current-goal goal)
  (cloel-agent--setup-buffer)
  (let* ((app-data (cloel-get-app-data "agent"))
         (proc (plist-get app-data :server-process)))
    (if (and proc (process-live-p proc))
        (cloel-agent-send-request)
      (cloel-agent-start-process))))

(provide 'cloel-agent)

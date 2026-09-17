;;; jb-kotlin-import.el --- Kotlin import status and recovery -*- lexical-binding: t; -*-

;;; Commentary:
;; Workspace-specific import state, logs and recovery commands.

;;; Code:

(require 'cl-lib)
(require 'lsp-mode)
(require 'jb-kotlin-reload)
(require 'button)
(declare-function jb-kotlin--initialization-options "jb-kotlin-lsp-dap")

(defcustom jb-kotlin-show-import-log-on-error t
  "Display the workspace import log when an import first fails."
  :type 'boolean :group 'jb-kotlin)

(cl-defstruct (jb-kotlin--import-state (:constructor jb-kotlin--make-import-state))
  (status 'unknown) failures blocked folders log view status-seen options)

(defvar jb-kotlin--import-states (make-hash-table :test 'eq)
  "Import state keyed by LSP workspace identity.")
(defvar-local jb-kotlin--import-buffer-workspace nil
  "Workspace whose import log or status this buffer displays.")
(defvar-local jb-kotlin--import-buffer-state nil
  "State retained in an import view after its workspace disconnects.")
(defvar-local jb-kotlin--import-indicator nil
  "Whether this buffer displays Kotlin import status in its mode line.")

(defun jb-kotlin--import-state (workspace)
  "Return or create WORKSPACE's import state."
  (or (gethash workspace jb-kotlin--import-states)
      (and (eq workspace jb-kotlin--import-buffer-workspace) jb-kotlin--import-buffer-state)
      (puthash workspace (jb-kotlin--make-import-state) jb-kotlin--import-states)))

(defun jb-kotlin--import-label (state)
  "Describe STATE without treating a reload response as import success."
  (cond ((eq 'stopped (jb-kotlin--import-state-status state)) "Stopped")
        ((jb-kotlin--import-state-blocked state)
         (if (cl-some (lambda (folder) (equal "ambiguousBuildSystem" (lsp-get folder :reason)))
                      (jb-kotlin--import-state-blocked state))
             "Build tool selection required" "Import blocked"))
        ((jb-kotlin--import-state-failures state) "Import failed")
        (t (pcase (jb-kotlin--import-state-status state)
             ('importing "Importing") ('ready "Ready") (_ "Import status unknown")))))

(defun jb-kotlin--import-context ()
  "Find the workspace for an import command, asking only if ambiguous."
  (or jb-kotlin--import-buffer-workspace
      (jb-kotlin--file-workspace buffer-file-name)
      (cl-find-if (lambda (w) (eq 'jb-kotlin (lsp--client-server-id (lsp--workspace-client w))))
                  (lsp-workspaces))
      (let ((choices
             (cl-loop for w in (lsp--session-workspaces (lsp-session))
                      when (and (eq 'initialized (lsp--workspace-status w))
                                (eq 'jb-kotlin (lsp--client-server-id (lsp--workspace-client w))))
                      collect (cons (lsp--workspace-root w) w))))
        (pcase choices
          (`() (user-error "No running Kotlin workspace"))
          (`((,_ . ,workspace)) workspace)
          (_ (cdr (assoc (completing-read "Kotlin workspace: " choices nil t) choices)))))))

(defvar jb-kotlin-import-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'jb-kotlin-import-status)
    (define-key map (kbd "l") #'jb-kotlin-show-import-log)
    (define-key map (kbd "r") #'jb-kotlin-retry-import)
    map))

(define-derived-mode jb-kotlin-import-mode special-mode "Kotlin Import"
  "View import status or logs.  Use r to retry, l for logs and g for status.")

(defun jb-kotlin--import-buffer (workspace state slot)
  "Return WORKSPACE's buffer in STATE's SLOT, either `log' or `view'."
  (let ((buffer (if (eq slot 'log) (jb-kotlin--import-state-log state)
                  (jb-kotlin--import-state-view state))))
    (unless (buffer-live-p buffer)
      (setq buffer (generate-new-buffer
                    (format "*Kotlin import%s: %s*" (if (eq slot 'log) " log" "")
                            (lsp--workspace-root workspace))))
      (with-current-buffer buffer
        (jb-kotlin-import-mode)
        (setq jb-kotlin--import-buffer-workspace workspace
              jb-kotlin--import-buffer-state state))
      (if (eq slot 'log) (setf (jb-kotlin--import-state-log state) buffer)
        (setf (jb-kotlin--import-state-view state) buffer)))
    buffer))

(defun jb-kotlin--import-render (workspace state)
  "Refresh an existing status view for WORKSPACE and STATE."
  (when-let* ((buffer (jb-kotlin--import-state-view state))
              (_ (buffer-live-p buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t) (position (point)))
        (erase-buffer)
        (insert (format "Kotlin workspace: %s\n\n%s\n\n"
                        (lsp--workspace-root workspace) (jb-kotlin--import-label state)))
        (insert-text-button "Open import log" 'action (lambda (_) (jb-kotlin-show-import-log workspace)))
        (insert "    ")
        (insert-text-button (if (jb-kotlin--import-state-blocked state) "Choose build tool / Retry" "Retry import")
                            'action (lambda (_) (jb-kotlin-retry-import workspace)))
        (insert "\n\n")
        (dolist (folder (jb-kotlin--import-state-blocked state))
          (insert (format "%s\n  %s; candidates: %s%s\n"
                          (lsp-get folder :folderUri) (lsp-get folder :reason)
                          (mapconcat #'identity (append (lsp-get folder :candidates) nil) ", ")
                          (if (eq t (lsp-get folder :dismissed)) " (selection dismissed)" ""))))
        (dolist (failure (jb-kotlin--import-state-failures state)) (insert failure "\n"))
        (dolist (folder (jb-kotlin--import-state-folders state))
          (insert (format "\n%s: %s (%s)\n%s\n"
                          (lsp-get folder :folderUri) (lsp-get folder :status)
                          (or (lsp-get folder :tool) "unknown tool")
                          (or (lsp-get folder :message) ""))))
        (goto-char (min position (point-max)))))))

(defun jb-kotlin--import-updated (workspace state previous)
  "Publish a change to WORKSPACE's STATE relative to PREVIOUS label."
  (let ((label (jb-kotlin--import-label state)))
    (unless (equal previous label)
      (message "Kotlin %s: %s" (lsp--workspace-root workspace) label)
      (when (and jb-kotlin-show-import-log-on-error (equal label "Import failed"))
        (display-buffer (jb-kotlin--import-buffer workspace state 'log))))
    (jb-kotlin--import-render workspace state)
    (force-mode-line-update t)))

(defun jb-kotlin--import-append (workspace state text)
  "Append TEXT to WORKSPACE's private log in STATE."
  (with-current-buffer (jb-kotlin--import-buffer workspace state 'log)
    (let ((inhibit-read-only t) (at-end (= (point) (point-max))))
      (save-excursion (goto-char (point-max)) (insert text "\n"))
      (when at-end (goto-char (point-max))))))

(defun jb-kotlin--import-log (workspace params)
  "Record log PARAMS and import outcome flags for WORKSPACE."
  (let* ((state (jb-kotlin--import-state workspace))
         (previous (jb-kotlin--import-label state))
         (text (or (lsp-get params :message) "")))
    (jb-kotlin--import-append workspace state text)
    (cond ((eq t (lsp-get params :failed))
           (cl-pushnew text (jb-kotlin--import-state-failures state) :test #'equal))
          ((eq t (lsp-get params :started))
           (setf (jb-kotlin--import-state-status state) 'importing))
          ((eq t (lsp-get params :succeeded))
           (setf (jb-kotlin--import-state-status state) 'ready)))
    (jb-kotlin--import-updated workspace state previous)))

(defun jb-kotlin--import-status-notification (workspace params)
  "Record blocked import folders in WORKSPACE from PARAMS."
  (let* ((state (jb-kotlin--import-state workspace))
         (previous (jb-kotlin--import-label state)))
    (setf (jb-kotlin--import-state-status-seen state) t
          (jb-kotlin--import-state-blocked state) (append (lsp-get params :blockedFolders) nil))
    (jb-kotlin--import-updated workspace state previous)))

(defun jb-kotlin--import-state-notification (workspace params)
  "Record WORKSPACE's import phase and per-folder results from PARAMS."
  (let* ((state (jb-kotlin--import-state workspace))
         (previous (jb-kotlin--import-label state))
         (folders (append (lsp-get params :folders) nil)))
    (setf (jb-kotlin--import-state-folders state) folders)
    (when (equal (lsp-get params :phase) "FINISHED")
      (cond
       ((cl-some (lambda (f) (equal "FAILED" (lsp-get f :status))) folders)
        (dolist (folder folders)
          (when (equal "FAILED" (lsp-get folder :status))
            (let ((failure (format "%s: %s" (lsp-get folder :folderUri)
                                   (or (lsp-get folder :message) "Import failed"))))
              (cl-pushnew failure (jb-kotlin--import-state-failures state) :test #'equal)
              (jb-kotlin--import-append workspace state failure)))))
       ((and folders (cl-every (lambda (f) (equal "SUCCESS" (lsp-get f :status))) folders))
        (setf (jb-kotlin--import-state-status state) 'ready
              (jb-kotlin--import-state-failures state) nil))))
    (jb-kotlin--import-updated workspace state previous)))

(defun jb-kotlin--import-initialized (workspace)
  "Seed WORKSPACE's blocked status without overwriting live notifications."
  (let ((state (jb-kotlin--import-state workspace)))
    (let ((source (cl-find-if #'buffer-live-p (lsp--workspace-buffers workspace))))
      (setf (jb-kotlin--import-state-options state)
            (if source (with-current-buffer source (jb-kotlin--initialization-options))
              (jb-kotlin--initialization-options))))
    (with-lsp-workspace workspace
      (lsp-request-async
       "intellij/workspaceImportStatus" (make-hash-table :test 'equal)
       (lambda (params)
         (when (and (eq state (gethash workspace jb-kotlin--import-states))
                    (not (jb-kotlin--import-state-status-seen state)))
           (jb-kotlin--import-status-notification workspace params)))
       :mode 'detached :error-handler #'ignore))))

(defun jb-kotlin--import-reloading (workspace options)
  "Start a new client-requested import cycle for WORKSPACE using OPTIONS."
  (let* ((state (jb-kotlin--import-state workspace))
         (previous (jb-kotlin--import-label state)))
    (setf (jb-kotlin--import-state-options state) options
          (jb-kotlin--import-state-status state) 'importing
          (jb-kotlin--import-state-failures state) nil
          (jb-kotlin--import-state-folders state) nil)
    (jb-kotlin--import-append workspace state "--- Reload requested ---")
    (jb-kotlin--import-updated workspace state previous)))

(defun jb-kotlin--import-reload-error (workspace error)
  "Record a reload request ERROR for WORKSPACE."
  (let* ((state (jb-kotlin--import-state workspace))
         (previous (jb-kotlin--import-label state))
         (text (format "Reload request failed: %s" error)))
    (push text (jb-kotlin--import-state-failures state))
    (jb-kotlin--import-append workspace state text)
    (jb-kotlin--import-updated workspace state previous)))

(defun jb-kotlin--import-disconnected (workspace)
  "Mark WORKSPACE stopped, retaining its log for diagnosis."
  (when-let* ((state (gethash workspace jb-kotlin--import-states)))
    (setf (jb-kotlin--import-state-status state) 'stopped)
    (jb-kotlin--import-render workspace state)
    (remhash workspace jb-kotlin--import-states)
    (force-mode-line-update t)))

;;;###autoload
(defun jb-kotlin-import-status (&optional workspace)
  "Show WORKSPACE's import status and recovery actions."
  (interactive)
  (let* ((workspace (or workspace (jb-kotlin--import-context)))
         (state (jb-kotlin--import-state workspace)))
    (pop-to-buffer (jb-kotlin--import-buffer workspace state 'view))
    (jb-kotlin--import-render workspace state)))

;;;###autoload
(defun jb-kotlin-show-import-log (&optional workspace)
  "Show WORKSPACE's import log.  Press r to retry the import."
  (interactive)
  (let ((workspace (or workspace (jb-kotlin--import-context))))
    (pop-to-buffer (jb-kotlin--import-buffer workspace (jb-kotlin--import-state workspace) 'log))))

;;;###autoload
(defun jb-kotlin-retry-import (&optional workspace)
  "Retry WORKSPACE's import; reopen its build-tool chooser if blocked."
  (interactive)
  (let* ((workspace (or workspace (jb-kotlin--import-context)))
         (state (jb-kotlin--import-state workspace)))
    (unless (eq 'initialized (lsp--workspace-status workspace))
      (user-error "Kotlin workspace stopped; start LSP in the project first"))
    ;; Log/status buffers have no directory-local settings. Reuse the latest
    ;; request's options, or the settings sent when the workspace initialized.
    (let ((reload (jb-kotlin--reload-queue workspace 'always)))
      (when-let* ((options (jb-kotlin--import-state-options state)))
        (setf (jb-kotlin--reload-state-options reload) options))
      (jb-kotlin--reload-flush workspace reload))))

(defun jb-kotlin--import-lighter ()
  "Return the current Kotlin buffer's import indicator."
  (when-let* ((workspace (cl-find-if
                         (lambda (w) (eq 'jb-kotlin (lsp--client-server-id (lsp--workspace-client w))))
                         (lsp-workspaces)))
              (state (gethash workspace jb-kotlin--import-states)))
    (concat " [Kotlin: " (jb-kotlin--import-label state) "]")))

(defun jb-kotlin--import-enable-indicator ()
  "Enable the indicator only in buffers managed by Kotlin LSP."
  (setq jb-kotlin--import-indicator
        (and lsp-managed-mode
             (cl-some (lambda (w) (eq 'jb-kotlin (lsp--client-server-id (lsp--workspace-client w))))
                      (lsp-workspaces)))))

(add-to-list 'minor-mode-alist '(jb-kotlin--import-indicator (:eval (jb-kotlin--import-lighter))))
(add-hook 'lsp-managed-mode-hook #'jb-kotlin--import-enable-indicator)
(add-hook 'jb-kotlin--reload-start-functions #'jb-kotlin--import-reloading)
(add-hook 'jb-kotlin--reload-error-functions #'jb-kotlin--import-reload-error)
(add-hook 'lsp-after-uninitialized-functions #'jb-kotlin--import-disconnected)

(provide 'jb-kotlin-import)
;;; jb-kotlin-import.el ends here

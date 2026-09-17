;;; jb-kotlin-reload.el --- Reload Kotlin build projects -*- lexical-binding: t; -*-

;;; Commentary:
;; Reload build descriptors on save without attaching build buffers to LSP.

;;; Code:

(require 'cl-lib)
(require 'lsp-mode)

(declare-function jb-kotlin--initialization-options "jb-kotlin-lsp-dap")
(declare-function jb-kotlin--workspace "jb-kotlin-lsp-dap")

(defcustom jb-kotlin-reload-on-save 'always
  "Reload policy for saved build descriptors.
`always' reloads automatically, `prompt' asks, and `never' leaves reloads
to `jb-kotlin-reload-workspace'.  Server-owned file watchers are unaffected."
  :type '(choice (const always) (const prompt) (const never))
  :group 'jb-kotlin)

(defcustom jb-kotlin-reload-delay 1.0
  "Idle seconds to combine build-file saves before a reload or prompt."
  :type 'number :group 'jb-kotlin)

(defconst jb-kotlin--build-file-names
  '("pom.xml" "build.gradle" "build.gradle.kts" "settings.gradle"
    "settings.gradle.kts" "BUILD" "BUILD.bazel" "MODULE.bazel"
    "WORKSPACE" "WORKSPACE.bazel" ".bazelproject")
  "Client-owned descriptors, matching upstream buildFiles.ts.
Do not include workspace.json or other server-watched import settings.")

(cl-defstruct (jb-kotlin--reload-state (:constructor jb-kotlin--make-reload-state))
  timer busy pending options policy)

(defvar jb-kotlin--reload-states (make-hash-table :test 'eq)
  "Pending reload state keyed by workspace.")
(defvar jb-kotlin--reload-start-functions nil
  "Functions called with workspace and initialization options before reload.")
(defvar jb-kotlin--reload-error-functions nil
  "Functions called with workspace and error after a failed reload request.")

(defun jb-kotlin--build-file-p (file)
  "Whether FILE is a supported local build descriptor."
  (and file (not (file-remote-p file))
       (let ((name (file-name-nondirectory file)))
         (or (member name jb-kotlin--build-file-names)
             (string-suffix-p ".bzl" name)))))

(defun jb-kotlin--file-workspace (file)
  "Find the initialized Kotlin workspace most closely containing FILE."
  (when (and file (not (file-remote-p file)))
    (let (owner (depth -1))
      (dolist (workspace (lsp--session-workspaces (lsp-session)))
        (when (and (eq 'initialized (lsp--workspace-status workspace))
                   (eq 'jb-kotlin (lsp--client-server-id
                                   (lsp--workspace-client workspace))))
          (dolist (root (or (lsp-workspace-folders workspace)
                            (list (lsp--workspace-root workspace))))
            (when (and root (not (file-remote-p root))
                       (> (length root) depth) (file-in-directory-p file root))
              (setq owner workspace depth (length root))))))
      owner)))

(defun jb-kotlin--reload-forget (workspace)
  "Cancel pending work and discard WORKSPACE's reload state."
  (when-let* ((state (gethash workspace jb-kotlin--reload-states)))
    (when-let* ((timer (jb-kotlin--reload-state-timer state)))
      (cancel-timer timer))
    (remhash workspace jb-kotlin--reload-states)))

(defun jb-kotlin--reload-schedule (workspace state)
  "Schedule the pending reload for WORKSPACE using STATE."
  (when-let* ((timer (jb-kotlin--reload-state-timer state)))
    (cancel-timer timer))
  (setf (jb-kotlin--reload-state-timer state)
        (run-with-idle-timer (max 0 jb-kotlin-reload-delay) nil
                             #'jb-kotlin--reload-flush workspace state)))

(defun jb-kotlin--reload-finished (workspace state error)
  "Finish a WORKSPACE request associated with STATE, reporting ERROR."
  (when (eq state (gethash workspace jb-kotlin--reload-states))
    (setf (jb-kotlin--reload-state-busy state) nil)
    (if error
        (progn
          (run-hook-with-args 'jb-kotlin--reload-error-functions workspace error)
          (message "Kotlin reload failed for %s: %s; retry with M-x jb-kotlin-reload-workspace"
                   (lsp--workspace-root workspace) error)))
    ;; A successful response does not imply a successful build import. Import
    ;; notifications own the user-visible outcome.
    ;; Only saves made during the request warrant another import, not errors.
    (when (jb-kotlin--reload-state-pending state)
      (jb-kotlin--reload-schedule workspace state))))

(defun jb-kotlin--reload-flush (workspace state)
  "Process a pending reload for WORKSPACE and STATE."
  (when (eq state (gethash workspace jb-kotlin--reload-states))
    (when-let* ((timer (jb-kotlin--reload-state-timer state)))
      (cancel-timer timer))
    (setf (jb-kotlin--reload-state-timer state) nil)
    (cond
     ((not (eq 'initialized (lsp--workspace-status workspace)))
      (jb-kotlin--reload-forget workspace))
     ((or (jb-kotlin--reload-state-busy state)
          (not (jb-kotlin--reload-state-pending state))) nil)
     (t
      ;; Mark busy before prompting: recursive edits may save more build files.
      (setf (jb-kotlin--reload-state-busy state) t)
      (let (submitted)
        (unwind-protect
            (when (and (not (eq 'never (jb-kotlin--reload-state-policy state)))
                       (or (eq 'always (jb-kotlin--reload-state-policy state))
                           (y-or-n-p (format "Reload Kotlin workspace %s after build-file changes? "
                                             (lsp--workspace-root workspace)))))
              (when (and (eq state (gethash workspace jb-kotlin--reload-states))
                         (eq 'initialized (lsp--workspace-status workspace)))
                (setq submitted t)
                (setf (jb-kotlin--reload-state-pending state) nil)
                (run-hook-with-args 'jb-kotlin--reload-start-functions workspace
                                    (jb-kotlin--reload-state-options state))
                (with-lsp-workspace workspace
                  (condition-case err
                      (lsp-request-async
                       "intellij/reloadWorkspace"
                       (list :initializationOptions (jb-kotlin--reload-state-options state))
                       (lambda (_) (jb-kotlin--reload-finished workspace state nil))
                       :mode 'detached
                       :error-handler (lambda (error)
                                        (jb-kotlin--reload-finished workspace state error)))
                    (error (jb-kotlin--reload-finished workspace state (error-message-string err)))))))
          ;; Declining or quitting the prompt must not leave the workspace busy.
          (unless submitted
            (setf (jb-kotlin--reload-state-pending state) nil
                  (jb-kotlin--reload-state-busy state) nil))))))))

(defun jb-kotlin--reload-queue (workspace policy)
  "Queue WORKSPACE with POLICY and the current initialization options."
  (let ((state (or (gethash workspace jb-kotlin--reload-states)
                   (puthash workspace (jb-kotlin--make-reload-state)
                            jb-kotlin--reload-states))))
    (setf (jb-kotlin--reload-state-options state) (jb-kotlin--initialization-options)
          (jb-kotlin--reload-state-policy state) policy
          (jb-kotlin--reload-state-pending state) t)
    (unless (jb-kotlin--reload-state-busy state)
      (jb-kotlin--reload-schedule workspace state))
    state))

(defun jb-kotlin--build-file-saved ()
  "Queue a reload when saving a build descriptor in a Kotlin workspace."
  (when (jb-kotlin--build-file-p buffer-file-name)
    (when-let* ((workspace (jb-kotlin--file-workspace buffer-file-name)))
      (jb-kotlin--reload-queue workspace jb-kotlin-reload-on-save))))

;;;###autoload
(defun jb-kotlin-reload-workspace ()
  "Asynchronously reload this Kotlin workspace with current settings.
Works in project build files even when their buffers do not run LSP."
  (interactive)
  (let* ((workspace (or (jb-kotlin--file-workspace buffer-file-name)
                        (jb-kotlin--workspace)))
         (state (jb-kotlin--reload-queue workspace 'always)))
    (jb-kotlin--reload-flush workspace state)))

(add-hook 'after-save-hook #'jb-kotlin--build-file-saved)
(add-hook 'lsp-after-uninitialized-functions #'jb-kotlin--reload-forget)

(provide 'jb-kotlin-reload)
;;; jb-kotlin-reload.el ends here

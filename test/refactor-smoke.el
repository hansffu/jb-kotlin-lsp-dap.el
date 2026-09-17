;;; refactor-smoke.el --- Live custom refactoring protocol test -*- lexical-binding: t; -*-

;; Use a generated navigation fixture and a NEW Emacs process.
(defvar jb-kotlin-navigation-smoke-no-run t)
(load (expand-file-name "navigation-smoke.el" (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defvar jb-refactor-done nil)
(defvar jb-refactor-error nil)

(defun jb-refactor-command (kind &rest properties)
  "Construct a server ModCommand of KIND with PROPERTIES."
  (append (list :kind (concat "com.jetbrains.ls.kotlinLsp.requests.core.ModCommandData." kind)) properties))

(defun jb-refactor-wait (predicate description &optional allow-error)
  "Wait for PREDICATE while checking protocol errors, reporting DESCRIPTION."
  (let ((deadline (+ (float-time) 90)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (unless allow-error (should-not jb-refactor-error))
      (accept-process-output nil 0.1))
    (unless allow-error (should-not jb-refactor-error))
    (should (funcall predicate)))
  (message "Verified: %s" description))

(defun jb-refactor-send (workspace command)
  "Ask WORKSPACE to execute a protocol-level COMMAND through its real dispatcher."
  (setq jb-refactor-done nil jb-refactor-error nil)
  (with-lsp-workspace workspace
		      (lsp-request-async "workspace/executeCommand"
					 (list :command "applyModCommand" :arguments (vector command))
					 (lambda (_) (setq jb-refactor-done t))
					 :mode 'detached :error-handler (lambda (error) (setq jb-refactor-error error)))))

(defun jb-refactor-smoke (conflicts)
  "Exercise real server notifications, and optionally CONFLICTS."
  (setq jb-refactor-done nil jb-refactor-error nil)
  (let* ((project (expand-file-name "sources" jb-kotlin-smoke-root))
         (default-directory (file-name-as-directory project))
         (file (expand-file-name "src/Refactor.kt" project))
         (original "fun value() = 1\n")
         (replacement "fun value() = 2\n")
         (kill-ring nil) (interprogram-cut-function nil)
         source workspace)
    (with-temp-file file (insert original))
    (setq source (find-file-noselect file))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (jb-kotlin-smoke-mode)
          (setq jb-kotlin-projects (vector (list :type "json" :path (lsp--path-to-uri project))))
          (lsp-workspace-folders-add project)
          (lsp)
          (jb-refactor-wait (lambda () (setq workspace (car (lsp-workspaces)))
                              (and workspace (eq 'initialized (lsp--workspace-status workspace)))) "workspace initialized")
          (message "Emacs PID=%d; LSP PID=%d; state=%s"
                   (emacs-pid) (process-id (lsp--workspace-proc workspace)) jb-kotlin-smoke-state)
          (jb-refactor-send workspace (jb-refactor-command "CopyToClipboard" :content "Kotlin clipboard smoke λ"))
          (jb-refactor-wait (lambda () (and jb-refactor-done (equal (car kill-ring) "Kotlin clipboard smoke λ")))
                            "server clipboard notification")
          (let ((renamed nil))
            (cl-letf (((symbol-function 'lsp-rename)
                       (lambda () (interactive) (setq renamed t))))
              (jb-refactor-send workspace (jb-refactor-command "StartRename"
                                                               :fileUrl (lsp--path-to-uri file)
                                                               :selectionStart 4 :selectionEnd 9))
              (jb-refactor-wait (lambda () (and jb-refactor-done renamed))
                                "server navigation and rename notification")))
          (when conflicts
            (dolist (decision '(cancel continue edit))
              (with-current-buffer source
		(erase-buffer) (insert original) (save-buffer))
              (jb-refactor-send
               workspace
               (jb-refactor-command "Composite"
                                    :commands (vector
                                               (jb-refactor-command "ShowConflicts"
                                                                    :conflicts (vector (list :messages ["Intentional test conflict"])))
                                               (jb-refactor-command "UpdateFileText"
                                                                    :fileUrl (lsp--path-to-uri file)
                                                                    :oldText original :newText replacement))))
              (jb-refactor-wait (lambda () (or jb-kotlin--conflicts jb-refactor-error))
				"conflict protocol probe" t)
              (when (and jb-refactor-error
			 (string-match-p "Serializer for subclass.*ModCommandData.ShowConflicts.*not found"
					 (lsp-get jb-refactor-error :message)))
		(ert-skip "Installed Kotlin LSP predates ShowConflicts; use a newer server"))
              (should-not jb-refactor-error)
              (let ((conflict (car jb-kotlin--conflicts)))
		(should (jb-kotlin--conflict-id conflict))
		(pcase decision
                  ('cancel (with-current-buffer (jb-kotlin--conflict-buffer conflict) (jb-kotlin-cancel-refactoring)))
                  ('continue (with-current-buffer (jb-kotlin--conflict-buffer conflict) (jb-kotlin-continue-refactoring)))
                  ('edit (with-current-buffer source (goto-char (point-max)) (insert "// intervening edit\n")))))
              (jb-refactor-wait (lambda () jb-refactor-done) (format "server completed %s decision" decision))
              (with-current-buffer source
		(should (equal (buffer-string)
                               (pcase decision ('continue replacement)
				      ('edit (concat original "// intervening edit\n")) (_ original)))))))
          (message "PASS: server clipboard and rename%s" (if conflicts "; conflict continue/cancel/edit" "")))
      (when workspace
        (when-let* ((log (lsp--workspace-ewoc workspace)))
          (with-current-buffer (ewoc-buffer log)
            (write-region (point-min) (point-max) (expand-file-name "lsp-io.log" jb-kotlin-smoke-state) nil 'silent)))
        (lsp-workspace-shutdown workspace)
        (let ((deadline (+ (float-time) 10)))
          (while (and (process-live-p (lsp--workspace-proc workspace)) (< (float-time) deadline))
            (accept-process-output nil 0.1))
          (when (process-live-p (lsp--workspace-proc workspace))
            (delete-process (lsp--workspace-proc workspace)))))
      (when (buffer-live-p source)
        (with-current-buffer source (set-buffer-modified-p nil))
        (kill-buffer source)))))

(ert-deftest jb-kotlin-live-refactoring-notifications () (jb-refactor-smoke nil))
(ert-deftest jb-kotlin-live-refactoring-conflicts () (jb-refactor-smoke t))
(ert-run-tests-batch-and-exit "jb-kotlin-live-refactoring-")

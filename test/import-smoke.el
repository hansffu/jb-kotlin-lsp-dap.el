;;; import-smoke.el --- Isolated import failure/recovery test -*- lexical-binding: t; -*-

(defvar jb-reload-smoke-no-run t)
(load (expand-file-name "reload-smoke.el" (file-name-directory (or load-file-name buffer-file-name))) nil t)

(setq jb-kotlin-show-import-log-on-error nil)
(defvar jb-import-smoke-choose nil)
(defvar jb-import-smoke-prompts 0)

;; Dismiss the first real server build-tool chooser, then choose Gradle on retry.
(advice-add 'lsp--window-log-message-request :around
            (lambda (original params)
              (let ((gradle (cl-find-if (lambda (action)
                                          (string-match-p "[Gg]radle" (lsp-get action :title)))
                                        (append (lsp-get params :actions) nil))))
                (if gradle
                    (progn (cl-incf jb-import-smoke-prompts)
                           (and jb-import-smoke-choose (lsp-get gradle :title)))
                  (funcall original params)))))

(defun jb-import-smoke-wait (predicate description)
  "Wait for PREDICATE and report DESCRIPTION, allowing expected build failures."
  (let ((deadline (+ (float-time) 180)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.2))
    (should (funcall predicate)))
  (message "Verified: %s" description))

(ert-deftest jb-kotlin-live-import-recovery ()
  (let* ((project (expand-file-name "groovy" jb-reload-root))
         (default-directory (file-name-as-directory project))
         (pom (expand-file-name "pom.xml" project))
         (source (find-file-noselect (expand-file-name "src/main/java/Main.kt" project)))
         (build (find-file-noselect (expand-file-name "build.gradle" project)))
         (initial "plugins { id(\"java\") }\n") workspace state)
    (with-current-buffer build (erase-buffer) (insert initial) (save-buffer))
    (with-temp-file pom
      (insert "<project><modelVersion>4.0.0</modelVersion><groupId>demo</groupId>"
              "<artifactId>ambiguous</artifactId><version>1</version></project>\n"))
    (unwind-protect
        (with-current-buffer source
          (jb-reload-smoke-mode)
          (setq jb-kotlin-projects [])
          (lsp-workspace-folders-add project)
          (lsp)
          (jb-import-smoke-wait
           (lambda () (setq workspace (car (lsp-workspaces)))
             (and workspace (eq 'initialized (lsp--workspace-status workspace)))) "workspace initialized")
          (setq state (jb-kotlin--import-state workspace))
          (message "Emacs PID=%d; LSP PID=%d; state=%s"
                   (emacs-pid) (process-id (lsp--workspace-proc workspace)) jb-reload-state)
          (jb-import-smoke-wait
           (lambda () (cl-some (lambda (folder) (eq t (lsp-get folder :dismissed)))
                               (jb-kotlin--import-state-blocked state))) "dismissed chooser is visible")
          (should (equal "Build tool selection required" (jb-kotlin--import-label state)))
          (save-window-excursion
            (jb-kotlin-import-status workspace)
            (should (string-match-p "selection dismissed" (buffer-string)))
            (setq jb-import-smoke-choose t)
            (jb-kotlin-retry-import))
          (jb-import-smoke-wait
           (lambda () (and (equal "Ready" (jb-kotlin--import-label state))
                           (not (jb-kotlin--reload-state-busy (gethash workspace jb-kotlin--reload-states)))))
           "chooser retry imported Gradle")
          (should (>= jb-import-smoke-prompts 2))
          (should jb-kotlin--import-indicator)
          (should (string-match-p "Ready" (jb-kotlin--import-lighter)))
          (with-current-buffer build
            (erase-buffer)
            (insert initial "throw new GradleException('intentional import failure')\n")
            (save-buffer))
          (jb-kotlin--reload-flush workspace (gethash workspace jb-kotlin--reload-states))
          (jb-import-smoke-wait
           (lambda () (and (equal "Import failed" (jb-kotlin--import-label state))
                           (not (jb-kotlin--reload-state-busy (gethash workspace jb-kotlin--reload-states)))))
           "failed import remains failed after the reload response")
          (with-current-buffer (jb-kotlin--import-state-log state)
            (should buffer-read-only)
            (should (string-match-p "intentional import failure" (buffer-string))))
          (with-current-buffer build
            (let ((jb-kotlin-reload-on-save 'never))
              (erase-buffer) (insert initial) (save-buffer)))
          (save-window-excursion
            (jb-kotlin-show-import-log workspace)
            (let ((jb-kotlin-projects [(:type "json" :path "file:///must-not-be-imported")]))
              (jb-kotlin-retry-import)))
          (jb-import-smoke-wait
           (lambda () (and (equal "Ready" (jb-kotlin--import-label state))
                           (not (jb-kotlin--reload-state-busy (gethash workspace jb-kotlin--reload-states)))))
           "retry from log recovered")
          (should-not (jb-kotlin--import-state-failures state))
          (should-not (jb-kotlin--import-state-blocked state))
          (should (string-match-p "Ready" (jb-kotlin--import-lighter)))
          (message "PASS: build-tool dismissal/retry, real import failure, log recovery and mode-line state"))
      (when workspace
        (when-let* ((log (lsp--workspace-ewoc workspace)))
          (with-current-buffer (ewoc-buffer log)
            (write-region (point-min) (point-max) (expand-file-name "lsp-io.log" jb-reload-state) nil 'silent)))
        (lsp-workspace-shutdown workspace)
        (let ((deadline (+ (float-time) 10)))
          (while (and (process-live-p (lsp--workspace-proc workspace)) (< (float-time) deadline))
            (accept-process-output nil 0.1))
          (when (process-live-p (lsp--workspace-proc workspace))
            (delete-process (lsp--workspace-proc workspace))))))))

(ert-run-tests-batch-and-exit 'jb-kotlin-live-import-recovery)

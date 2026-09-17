;;; reload-smoke.el --- Isolated live build reload test -*- lexical-binding: t; -*-

;; Run in a NEW batch Emacs, never emacsclient. See test/README.org.
(require 'jb-kotlin-lsp-dap)
(require 'lsp-lens)
(require 'lsp-modeline)
(require 'lsp-headerline)
(require 'lsp-diagnostics)
(require 'lsp-completion)
(require 'ert)

(defvar jb-reload-root (file-name-as-directory (or (getenv "JB_KOTLIN_TEST_ROOT")
                                                 (error "Set JB_KOTLIN_TEST_ROOT"))))
(defvar jb-reload-kind (or (getenv "JB_KOTLIN_TEST_PROJECT") "groovy"))
(defvar jb-reload-state (file-name-as-directory
                        (make-temp-file (expand-file-name "emacs-" jb-reload-root) t)))
(defvar jb-reload-imports 0)
(defvar jb-reload-requests 0)
(defvar jb-reload-errors nil)

(make-directory (expand-file-name "gradle" jb-reload-state))
(setq user-emacs-directory jb-reload-state
      lsp-session-file (expand-file-name "session" jb-reload-state)
      lsp-auto-guess-root nil lsp-enable-file-watchers nil
      lsp-enable-snippet nil lsp-enable-dap-auto-configure nil
      lsp-enable-indentation nil lsp-enable-on-type-formatting nil
      lsp-enabled-clients '(jb-kotlin) lsp-response-timeout 180
      lsp-log-io t lsp-restart 'ignore
      jb-kotlin-reload-on-save 'always
      jb-kotlin-default-sdk (string-trim (with-temp-buffer
                                          (insert-file-contents (expand-file-name "jdk-home" jb-reload-root))
                                          (buffer-string)))
      jb-kotlin-server-command
      (list "kotlin-lsp" "--stdio"
            (concat "--system-path=" (expand-file-name "system" jb-reload-state))))
(setenv "IJ_JAVA_OPTIONS"
        (concat (getenv "IJ_JAVA_OPTIONS")
                " -Didea.config.path=" (expand-file-name "config" jb-reload-state)
                " -Didea.log.path=" (expand-file-name "log" jb-reload-state)
                " -Djava.util.prefs.userRoot=" (expand-file-name "prefs" jb-reload-state)
                " -Dcom.jetbrains.ls.imports.gradle.gradleUserHome="
                (expand-file-name "gradle" jb-reload-state)))

(define-derived-mode jb-reload-smoke-mode prog-mode "Kotlin-reload-smoke")
(add-to-list 'lsp-language-id-configuration '(jb-reload-smoke-mode . "kotlin"))
(push 'jb-reload-smoke-mode (lsp--client-major-modes (gethash 'jb-kotlin lsp-clients)))
(advice-add 'jb-kotlin--import-log :before
            (lambda (_workspace params)
              (when (eq t (lsp-get params :succeeded)) (cl-incf jb-reload-imports))
              (when (eq t (lsp-get params :failed)) (push params jb-reload-errors))
              (message "Import: %S" params)))
(advice-add 'lsp-request-async :before
            (lambda (method &rest _)
              (when (equal method "intellij/reloadWorkspace") (cl-incf jb-reload-requests))))
(advice-add 'jb-kotlin--reload-finished :before
            (lambda (_workspace _state error) (when error (push error jb-reload-errors))))

(defun jb-reload-wait (predicate description)
  "Wait for PREDICATE, failing after 180 seconds with DESCRIPTION."
  (let ((deadline (+ (float-time) 180)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (should-not jb-reload-errors)
      (accept-process-output nil 0.2))
    (should (funcall predicate)))
  (message "Ready: %s" description))

(defun jb-reload-definition (name)
  "Request the definition of NAME in the current Kotlin buffer."
  (goto-char (point-min)) (search-forward name) (backward-char (length name))
  (lsp-request "textDocument/definition" (lsp--text-document-position-params)))

(defun jb-reload-save (buffer text workspace)
  "Save TEXT to BUFFER and exercise WORKSPACE's queued reload."
  (with-current-buffer buffer
    (erase-buffer) (insert text) (save-buffer))
  (let* ((state (gethash workspace jb-kotlin--reload-states))
         (timer (jb-kotlin--reload-state-timer state)))
    (should (timerp timer))
    ;; Batch Emacs has no command-loop idle periods. Fire the scheduled callback
    ;; explicitly; the save hook, request, build import and resolution are real.
    (jb-kotlin--reload-flush workspace state)
    (jb-reload-wait (lambda () (not (jb-kotlin--reload-state-busy state))) "reload response")
    (should-not jb-reload-errors)))

(ert-deftest jb-kotlin-live-build-reload ()
  (let* ((project (expand-file-name jb-reload-kind jb-reload-root))
         (default-directory (file-name-as-directory project))
         (source (find-file-noselect (expand-file-name "src/main/java/Main.kt" project)))
         (build (find-file-noselect
                 (expand-file-name (if (equal jb-reload-kind "kotlin") "build.gradle.kts" "build.gradle") project)))
         (initial "plugins { id(\"java\") }\n") workspace)
    (unwind-protect
        (with-current-buffer source
          (jb-reload-smoke-mode)
          (setq jb-kotlin-projects (vector (list :type "gradle" :path (lsp--path-to-uri project)
                                                :offline t)))
          (lsp-workspace-folders-add project)
          (lsp)
          (jb-reload-wait (lambda () (setq workspace (car (lsp-workspaces)))
                            (and workspace (eq 'initialized (lsp--workspace-status workspace))))
                          "workspace initialized")
          (princ (format "Emacs PID=%d; LSP PID=%d; state=%s\n"
                         (emacs-pid) (process-id (lsp--workspace-proc workspace)) jb-reload-state))
          (jb-reload-wait (lambda () (> jb-reload-imports 0)) "initial build import")
          (jb-reload-wait (lambda () (> (length (jb-reload-definition "ArrayList")) 0)) "JDK resolution")
          (should (= 0 (length (jb-reload-definition "Greeter"))))
          (should-not (buffer-local-value 'lsp-mode build))
          (jb-reload-save build (concat initial "dependencies { implementation(files(\"../greeter.jar\")) }\n") workspace)
          (jb-reload-wait (lambda () (> (length (jb-reload-definition "Greeter")) 0)) "new dependency resolves")
          (should (= 1 jb-reload-requests))
          (jb-reload-save build initial workspace)
          (jb-reload-wait (lambda () (= 0 (length (jb-reload-definition "Greeter")))) "removed dependency is unresolved")
          (should (= 2 jb-reload-requests))
          (princ (format "PASS: %s save adds and removes a real dependency, two reload requests\n" jb-reload-kind)))
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

(ert-run-tests-batch-and-exit 'jb-kotlin-live-build-reload)

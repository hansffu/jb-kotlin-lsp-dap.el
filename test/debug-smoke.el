;;; debug-smoke.el --- Isolated real Kotlin debugger sessions -*- lexical-binding: t; -*-
(defvar jb-kotlin-navigation-smoke-no-run t)
(load (expand-file-name "navigation-smoke.el" (file-name-directory load-file-name)) nil t)
(setq dap-breakpoints-file (expand-file-name "breakpoints" jb-kotlin-smoke-state))
(require 'dap-ui)
(require 'dap-launch)
(require 'dap-variables)

(defvar jb-debug-kind (or (getenv "JB_KOTLIN_TEST_PROJECT") "gradle"))
(defvar jb-debug-output "")
(defvar jb-debug-stops 0)
(defvar jb-debug-session nil)
(defvar jb-debug-indexed nil)
(puthash "intellij/ready-for-test" (lambda (&rest _) (setq jb-debug-indexed t))
         (lsp--client-notification-handlers (gethash 'jb-kotlin lsp-clients)))
(make-directory (expand-file-name "gradle" jb-kotlin-smoke-state) t)
(setenv "GRADLE_USER_HOME" (expand-file-name "gradle" jb-kotlin-smoke-state))
(setq jb-kotlin-request-timeout 180 lsp-response-timeout 180 dap-inhibit-io nil)
(setenv "IJ_JAVA_OPTIONS"
        (concat (getenv "IJ_JAVA_OPTIONS")
                " -Dcom.jetbrains.ls.imports.gradle.gradleUserHome=" (expand-file-name "gradle" jb-kotlin-smoke-state)
                " -Dmaven.repo.local=" (expand-file-name "repository" jb-kotlin-smoke-root)))

(advice-add 'dap--on-event :before
            (lambda (_session event)
              (message "DAP event: %s %s" (gethash "event" event) (if (equal (gethash "event" event) "output") (gethash "output" (gethash "body" event)) ""))
              (pcase (gethash "event" event)
                ("output" (setq jb-debug-output (concat jb-debug-output (gethash "output" (gethash "body" event)))))
                ("stopped" (cl-incf jb-debug-stops)))))

(defun jb-debug-wait (predicate label &optional seconds)
  "Wait for PREDICATE to verify LABEL within SECONDS."
  (let ((deadline (+ (float-time) (or seconds 180))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (when (and jb-debug-session (eq 'failed (dap--debug-session-state jb-debug-session)))
        (error "DAP failed: %s" (dap--debug-session-error-message jb-debug-session)))
      (when (and jb-debug-session (eq 'terminated (dap--debug-session-state jb-debug-session)))
        (error "Terminated before %s; output: %s" label jb-debug-output))
      (accept-process-output nil 0.1))
    (should (funcall predicate)))
  (message "Verified: %s" label))

(defun jb-debug-request (command args)
  "Send COMMAND and ARGS to the real debug adapter and return its body."
  (let (response)
    (dap--send-message (dap--make-request command args)
                       (lambda (result) (setq response result)) jb-debug-session)
    (jb-debug-wait (lambda () response) command 30)
    (should (eq t (gethash "success" response)))
    (gethash "body" response)))

(defun jb-debug-start (config)
  "Launch CONFIG and wait for the adapter session."
  (setq jb-debug-output "" jb-debug-stops 0 jb-debug-session nil)
  (let ((previous (dap--cur-session)))
    (dap-debug config)
    (jb-debug-wait (lambda ()
                     (let ((session (dap--cur-session)))
                       (when (and session (not (eq previous session)))
                         (setq jb-debug-session session)))) "DAP session")))

(defun jb-debug-inspect-and-continue ()
  "Check the breakpoint, local variables and stepping, then continue."
  (jb-debug-wait (lambda () (> jb-debug-stops 0)) "breakpoint hit")
  (let* ((thread (dap--debug-session-thread-id jb-debug-session))
         (stack (gethash "stackFrames" (jb-debug-request "stackTrace" (list :threadId thread))))
         (frame (elt stack 0))
         (scopes (gethash "scopes" (jb-debug-request "scopes" (list :frameId (gethash "id" frame)))))
         variables)
    (should (= 5 (gethash "line" frame)))
    (jb-debug-wait
     (lambda ()
       (setq variables nil)
       (mapc (lambda (scope)
               (setq variables
                     (append variables
                             (append (gethash "variables"
                                              (jb-debug-request "variables"
                                                                (list :variablesReference (gethash "variablesReference" scope)))) nil)))) scopes)
       (seq-some (lambda (v) (and (equal "greeting" (gethash "name" v))
                                  (string-match-p "debug-ready" (gethash "value" v)))) variables))
     "local variable value" 30)
    (when (equal (plist-get (dap--debug-session-launch-args jb-debug-session) :request) "launch")
      (should (string-match-p "space arg"
                (gethash "result" (jb-debug-request "evaluate"
                                   (list :frameId (gethash "id" frame) :context "watch" :expression "args[0]"))))))
    (let ((previous jb-debug-stops))
      (jb-debug-request "next" (list :threadId thread))
      (jb-debug-wait (lambda () (> jb-debug-stops previous)) "step over"))
    (jb-debug-request "continue" (list :threadId thread)))
  (jb-debug-wait (lambda () (eq 'terminated (dap--debug-session-state jb-debug-session))) "program terminated"))

(ert-deftest jb-kotlin-live-debug ()
  (let* ((project (expand-file-name jb-debug-kind jb-kotlin-smoke-root))
         (default-directory (file-name-as-directory project))
         (file (expand-file-name "src/main/kotlin/demo/Main.kt" project))
         (source (find-file-noselect file))
         (type (if (equal jb-debug-kind "gradle") "intellij_gradle" "intellij_jvm"))
         (config (list :type type :request "launch" :name "Kotlin smoke" :mainClass "demo.MainKt"
                        :file file :cwd project :args ["space arg"] :env (ht ("JB_DEBUG_VALUE" "env value"))
                        :console "internalConsole")) workspace target-process target-buffer)
    (unwind-protect
        (progn
          (switch-to-buffer source) (jb-kotlin-smoke-mode)
          (setq jb-kotlin-projects (vector (list :type jb-debug-kind :path (lsp--path-to-uri (if (equal jb-debug-kind "maven") (expand-file-name "pom.xml" project) project)) :offline t)))
          (lsp-workspace-folders-add project) (lsp)
          (jb-debug-wait (lambda () (setq workspace (car (lsp-workspaces)))
                           (and workspace (eq 'initialized (lsp--workspace-status workspace)))) "workspace initialized")
          (jb-debug-wait (lambda ()
                           (let ((status (jb-kotlin--import-state-status (jb-kotlin--import-state workspace))))
                             (when (jb-kotlin--import-state-failures (jb-kotlin--import-state workspace)) (error "Project import failed; see %s" jb-kotlin-smoke-state))
                             (eq status 'ready))) "project imported")
          (jb-debug-wait
           (lambda ()
             (condition-case err
                 (jb-kotlin--command "intellij.java.resolveLaunch" (list :uri (lsp--path-to-uri file)))
               (error (message "Waiting for module: %s" (error-message-string err)) nil)))
           "launch module available" 30)
          (jb-debug-wait (lambda () jb-debug-indexed) "server indexing ready")
          (message "Detected main classes: %S" (jb-kotlin--main-classes))
          ;; Round-trip the generated portable configuration through dap-mode's
          ;; real launch.json parser and variable expansion.
          (let ((launch-file (expand-file-name ".vscode/launch.json" project)))
            (make-directory (file-name-directory launch-file) t)
            (with-temp-buffer
              (insert "{\"version\":\"0.2.0\",\"configurations\":[]}")
              (jb-kotlin--insert-launch
               (plist-put (plist-put (copy-sequence config) :file "${workspaceFolder}/src/main/kotlin/demo/Main.kt")
                          :cwd "${workspaceFolder}"))
              (write-region (point-min) (point-max) launch-file nil 'silent))
            (setq config (cdr (car (dap-launch-find-parse-launch-json)))))
          (goto-char (point-min)) (forward-line 4) (dap-breakpoint-toggle)
          (jb-debug-start config)
          (jb-debug-inspect-and-continue)
          (message "Internal console output: %s" jb-debug-output)
          ;; Launch without debugging in the real integrated console.
          (jb-debug-start (plist-put (plist-put (plist-put (copy-tree config) :noDebug t)
                                                  :console "integratedTerminal") :args ["stdin" "space arg"]))
          (let (process buffer)
            (jb-debug-wait (lambda () (setq process (car (gethash jb-debug-session jb-kotlin--terminal-processes)))) "interactive console created")
            (setq buffer (process-buffer process))
            (jb-debug-wait (lambda () (with-current-buffer buffer (string-match-p "env=env value" (buffer-string)))) "console environment")
            (process-send-string process "typed input\n")
            (jb-debug-wait (lambda () (with-current-buffer buffer (string-match-p "input=typed input" (buffer-string)))) "interactive stdin")
            (jb-debug-wait (lambda () (eq 'terminated (dap--debug-session-state jb-debug-session))) "run terminated"))
          (setq jb-debug-session nil)
          ;; Attach to a separately started, suspended JVM using an ephemeral JDWP port.
          (let* ((jb-kotlin-build-before-run nil)
                 (resolved (jb-kotlin--resolve-jvm (list :mainClass "demo.MainKt") (lsp--path-to-uri file)))
                 (classpath (mapconcat #'identity (append (plist-get resolved :classPaths) nil) path-separator)))
            (setq target-buffer (generate-new-buffer "*Kotlin attach target*"))
            (setq target-process
                  (make-process :name "Kotlin attach target" :buffer target-buffer :noquery t
                                :command (list (plist-get resolved :javaExec)
                                               "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=127.0.0.1:0"
                                               "-cp" classpath "demo.MainKt")))
            (let (port)
              (jb-debug-wait (lambda () (with-current-buffer target-buffer
                                         (goto-char (point-min))
                                         (when (re-search-forward "Listening for transport dt_socket at address: \\([0-9]+\\)" nil t)
                                           (setq port (string-to-number (match-string 1)))))) "JDWP listener")
              (jb-debug-start (list :type "intellij_jvm" :request "attach" :name "Attach smoke" :hostName "127.0.0.1" :port port))
              (jb-debug-inspect-and-continue)))
          (message "PASS: %s launch, breakpoints, variables, stepping, run with stdin and attach" jb-debug-kind))
      (when (and target-process (process-live-p target-process)) (delete-process target-process))
      (when workspace
        (when-let* ((log (lsp--workspace-ewoc workspace)))
          (with-current-buffer (ewoc-buffer log)
            (write-region (point-min) (point-max) (expand-file-name "lsp-io.log" jb-kotlin-smoke-state) nil 'silent)))
        (dolist (session (dap--get-sessions))
          (jb-kotlin--terminal-cleanup session)
          (when (process-live-p (dap--debug-session-proc session)) (delete-process (dap--debug-session-proc session))))
        (lsp-workspace-shutdown workspace)))))

(ert-run-tests-batch-and-exit 'jb-kotlin-live-debug)

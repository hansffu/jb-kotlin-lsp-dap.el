;;; jb-kotlin-test.el --- Protocol contract tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'jb-kotlin-lsp-dap)

(defmacro jb-kotlin-test--object (&rest entries)
  "Construct protocol data from ENTRIES in lsp-mode's configured format."
  `(if lsp-use-plists
       (list ,@(cl-loop for (key value) in entries
                       append (list (intern (concat ":" key)) value)))
     (lsp-ht ,@entries)))

(defmacro jb-kotlin-test--workspace (&rest body)
  "Evaluate BODY in an isolated, real lsp-mode workspace object."
  (declare (indent 0))
  `(let* ((client (gethash 'jb-kotlin lsp-clients))
          (workspace (make-lsp--workspace :client client :root "/tmp/project"))
          (lsp--buffer-workspaces (list workspace))
          (lsp--cur-workspace nil))
     ,@body))

(ert-deftest jb-kotlin-initialization-contract ()
  (let* ((jb-kotlin-default-sdk "/tmp/jdk")
         (options (jb-kotlin--initialization-options))
         ;; Inspect the actual JSON types, particularly false and empty arrays.
         (json (json-parse-string (json-encode options))))
    (should (eq t (gethash "runMainCodeLens" json)))
    (should (eq t (gethash "lazyIntentions" json)))
    (should (eq :false (gethash "intellijExtensions" json)))
    (should (equal [] (gethash "projects" json)))
    (should (hash-table-p (gethash "buildTools" json)))
    (should (equal "/tmp/jdk" (gethash "defaultSdk" json)))))

(ert-deftest jb-kotlin-command-is-scoped-and-uses-array ()
  (jb-kotlin-test--workspace
    (cl-letf (((symbol-function 'lsp-request)
               (lambda (method params)
                 (should (eq workspace lsp--cur-workspace))
                 (should (equal method "workspace/executeCommand"))
                 (should (equal params '(:command "example" :arguments ["one"])))
                 123)))
      (should (= 123 (jb-kotlin--command "example" "one"))))))

(ert-deftest jb-kotlin-rejects-missing-workspace ()
  (let ((lsp--cur-workspace nil) (lsp--buffer-workspaces nil))
    (should-error (jb-kotlin--workspace) :type 'user-error)))

(ert-deftest jb-kotlin-attach-keeps-jdwp-and-adapter-ports-separate ()
  (jb-kotlin-test--workspace
    (cl-letf (((symbol-function 'jb-kotlin--command)
               (lambda (command &rest args)
                 (should (equal command "start_debug_server"))
                 (should (equal args '("file:///tmp/project")))
                 45678)))
      (let ((config (jb-kotlin--debug-config
                     '(:type "intellij_jvm" :request "attach"
                       :hostName "remote.example" :port 5005))))
        (should (= 45678 (plist-get config :debugServer)))
        (should (= 5005 (plist-get config :port)))
        (should (equal "localhost" (plist-get config :host)))
        (should (equal "remote.example" (plist-get config :hostName)))))))

(ert-deftest jb-kotlin-rejects-invalid-adapter-port ()
  (jb-kotlin-test--workspace
    (dolist (port '(nil "1234" 0 -1 65536))
      (cl-letf (((symbol-function 'jb-kotlin--command) (lambda (&rest _) port)))
        (should-error (jb-kotlin--debug-config '(:request "attach"))
                      :type 'user-error)))))

(ert-deftest jb-kotlin-jvm-builds-before-resolving-and-forwards-paths ()
  (jb-kotlin-test--workspace
    (let ((jb-kotlin-build-before-run t)
          (original '(:type "intellij_jvm" :request "launch"
                      :mainClass "demo.MainKt" :file "/tmp/project/Main.kt"
                      :vmArgs ["-Xmx1g"]))
          built)
      (cl-letf (((symbol-function 'jb-kotlin--build)
                 (lambda (uri)
                   (should (equal uri "file:///tmp/project/Main.kt"))
                   (setq built t)))
                ((symbol-function 'jb-kotlin--command)
                 (lambda (command &rest args)
                   (cond
                    ((equal command "start_debug_server") 45678)
                    ((equal command "intellij.java.resolveLaunch")
                     (should built)
                     (should (equal ["-Xmx1g"]
                                    (gethash "vmArgs" (plist-get (car args) :overrides))))
                     (jb-kotlin-test--object ("classpath" ["classes"])
                             ("modulePath" ["modules"])
                             ("moduleContentPaths" ["resources"])
                             ("moduleName" "demo") ("javaExec" "/jdk/bin/java")
                             ("workingDirectory" "/tmp/project")
                             ("vmArgs" ["-Xmx1g" "--enable-preview"])))
                    (t (ert-fail (format "Unexpected command: %s" command)))))))
        (let ((config (jb-kotlin--debug-config original)))
          (should (equal ["classes"] (plist-get config :classPaths)))
          (should (equal ["modules"] (plist-get config :modulePaths)))
          (should (equal ["resources"] (plist-get config :moduleContentPaths)))
          (should (equal "demo" (plist-get config :moduleName)))
          (should (equal "/jdk/bin/java" (plist-get config :javaExec)))
          (should (equal "/tmp/project" (plist-get config :cwd)))
          (should (equal ["-Xmx1g" "--enable-preview"] (plist-get config :vmArgs)))
          (should (equal "internalConsole" (plist-get config :console)))
          (should-not (plist-member original :debugServer)))))))

(ert-deftest jb-kotlin-gradle-passes-target-without-separate-build ()
  (jb-kotlin-test--workspace
    (cl-letf (((symbol-function 'jb-kotlin--build)
               (lambda (&rest _) (ert-fail "Gradle must build in adapter")))
              ((symbol-function 'jb-kotlin--command)
               (lambda (command &rest _)
                 (cond ((equal command "start_debug_server") 45678)
                       ((equal command "intellij.java.resolveBuildToolLaunch")
                        (jb-kotlin-test--object ("tool" "gradle") ("moduleName" "app")
                                ("scopeClassPaths" ["app/classes"])))
                       (t (ert-fail command))))))
      (let* ((config (jb-kotlin--debug-config
                      '(:type "intellij_gradle" :request "launch"
                        :mainClass "demo.MainKt" :file "/tmp/project/Main.kt"
                        :projectPath ":app" :sourceSet "main"
                        :gradleArgs ["--offline"])))
             (target (plist-get config :buildToolTarget)))
        (should (equal "file:///tmp/project/Main.kt" (plist-get target :uri)))
        (should (equal "app" (plist-get target :moduleName)))
        (should (equal ":app" (plist-get target :projectPath)))
        (should (equal "main" (plist-get target :sourceSet)))
        (should (equal ["--offline"] (plist-get target :toolArgs)))
        (should (equal ["app/classes"] (plist-get config :classPaths)))))))

(ert-deftest jb-kotlin-gradle-rejects-other-build-tools ()
  (cl-letf (((symbol-function 'jb-kotlin--command)
             (lambda (&rest _) (jb-kotlin-test--object ("tool" "maven")))))
    (should-error (jb-kotlin--resolve-gradle '(:mainClass "MainKt") "file:///x")
                  :type 'user-error)))

(ert-deftest jb-kotlin-class-only-launch-resolves-source ()
  (cl-letf (((symbol-function 'jb-kotlin--command)
             (lambda (command &rest args)
               (should (equal command "intellij.java.resolveClassDocument"))
               (should (equal args '((:fqn "demo.MainKt"))))
               (jb-kotlin-test--object ("uri" "file:///source.kt")))))
    (should (equal "file:///source.kt"
                   (jb-kotlin--target-uri '(:mainClass "demo.MainKt"))))))

(ert-deftest jb-kotlin-lens-dispatches-locally-with-json-false ()
  (jb-kotlin-test--workspace
    (cl-letf (((symbol-function 'jb-kotlin--command)
               (lambda (&rest _) (jb-kotlin-test--object ("tool" "gradle"))))
              ((symbol-function 'dap-debug)
               (lambda (config)
                 (should (equal "intellij_gradle" (plist-get config :type)))
                 (should (eq :json-false (plist-get config :noDebug)))
                 (should (equal "/tmp/Main.kt" (plist-get config :file))))))
      (lsp--execute-command
       (jb-kotlin-test--object ("command" "intellij.jvm.runMain")
               ("arguments" (vector (jb-kotlin-test--object ("mainClass" "MainKt")
                                           ("uri" "file:///tmp/Main.kt")
                                           ("noDebug" :json-false)))))))))

(ert-deftest jb-kotlin-build-failure-stops-launch-before-adapter-start ()
  (jb-kotlin-test--workspace
    (cl-letf (((symbol-function 'jb-kotlin--build)
               (lambda (_) (user-error "Build failed")))
              ((symbol-function 'jb-kotlin--command)
               (lambda (&rest _) (ert-fail "Must stop before adapter starts"))))
      (should-error
       (jb-kotlin--debug-config '(:type "intellij_jvm" :request "launch"
                                 :mainClass "MainKt" :file "/tmp/Main.kt"))
       :type 'user-error))))

(ert-deftest jb-kotlin-build-runs-real-process-and-checks-exit-status ()
  (dolist (exit-code '(0 7))
    (cl-letf (((symbol-function 'jb-kotlin--command)
               (lambda (&rest _)
                 (jb-kotlin-test--object ("supported" t) ("cwd" "/tmp")
                         ("command" (vector shell-file-name "-c"
                                            (format "exit %d" exit-code))))))
              ((symbol-function 'display-buffer) #'ignore))
      (if (zerop exit-code)
          (jb-kotlin--build "file:///tmp/Main.kt")
        (should-error (jb-kotlin--build "file:///tmp/Main.kt") :type 'user-error)))))

(ert-deftest jb-kotlin-unsupported-build-does-not-start-process ()
  (cl-letf (((symbol-function 'jb-kotlin--command)
             (lambda (&rest _) (jb-kotlin-test--object ("supported" :json-false))))
            ((symbol-function 'make-process)
             (lambda (&rest _) (ert-fail "Unsupported builds must not spawn"))))
    (jb-kotlin--build "file:///tmp/Main.kt")))

(load (expand-file-name "jb-kotlin-navigation-test.el"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t)
(load (expand-file-name "jb-kotlin-reload-test.el"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t)

;;; jb-kotlin-test.el ends here

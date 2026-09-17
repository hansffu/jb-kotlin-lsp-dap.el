;;; jb-kotlin-lsp-dap.el --- JetBrains Kotlin LSP and DAP integration -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (lsp-mode "9.0.0") (dap-mode "0.8"))
;; Keywords: languages, tools

;;; Commentary:
;; Require this package, then use `lsp-deferred' in Kotlin buffers.
;; `jb-kotlin-debug' launches a main class; `jb-kotlin-attach' attaches
;; to a JVM.  The language server supplies its own TCP debug adapter.
;; See README.org for installation and the upstream protocol reference.

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'lsp-mode)
(require 'dap-mode)
(require 'jb-kotlin-navigation)

(defgroup jb-kotlin nil
  "JetBrains Kotlin language server and debugger."
  :group 'lsp-mode :prefix "jb-kotlin-")

(defcustom jb-kotlin-server-command '("kotlin-lsp" "--stdio")
  "Command and arguments for the standalone Kotlin language server."
  :type '(repeat string))

(defcustom jb-kotlin-default-sdk nil
  "JDK home used for project symbol resolution, or nil for server defaults."
  :type '(choice (const nil) directory))

(defcustom jb-kotlin-projects []
  "Vector of project import plists, as accepted by Kotlin LSP.
For example, [(:type \"gradle\" :path \"file:///path/to/project\")]."
  :type 'sexp)

(defcustom jb-kotlin-build-tools (make-hash-table :test 'equal)
  "Map from workspace URI strings to build tool names."
  :type 'sexp)

(defcustom jb-kotlin-build-before-run t
  "Whether JVM launches run the build command supplied by the server.
Gradle launches always build through the debug adapter."
  :type 'boolean)

(defcustom jb-kotlin-request-timeout 60
  "Seconds to wait for launch and build resolution requests."
  :type 'number)

(defun jb-kotlin--initialization-options ()
  "Return only extension flags that this client implements."
  (append (list :lazyIntentions t :runMainCodeLens t
                :intellijExtensions :json-false
                :projects jb-kotlin-projects :buildTools jb-kotlin-build-tools)
          (when jb-kotlin-default-sdk
            (list :defaultSdk (expand-file-name jb-kotlin-default-sdk)))))

(defun jb-kotlin--workspace ()
  "Return the Kotlin workspace for the current buffer, or signal an error."
  (or (cl-find-if
       (lambda (workspace)
         (eq (lsp--client-server-id (lsp--workspace-client workspace))
             'jb-kotlin))
       (lsp-workspaces))
      (user-error "Start JetBrains Kotlin LSP in this buffer first (M-x lsp)")))

(defun jb-kotlin--command (command &rest arguments)
  "Execute server COMMAND with ARGUMENTS in the current Kotlin workspace."
  (with-lsp-workspace (jb-kotlin--workspace)
    (let ((lsp-response-timeout jb-kotlin-request-timeout))
      (lsp-request "workspace/executeCommand"
                   (list :command command :arguments (vconcat arguments))))))

(defun jb-kotlin--target-uri (config)
  "Find the source URI for launch CONFIG."
  (or (when-let* ((file (plist-get config :file)))
        (lsp--path-to-uri (expand-file-name file)))
      (lsp-get (jb-kotlin--command
                "intellij.java.resolveClassDocument"
                (list :fqn (plist-get config :mainClass))) :uri)
      (user-error "The server could not find the main class source")))

(defun jb-kotlin--build (uri)
  "Build the module containing URI, stopping a launch on failure.
Output goes to a compilation buffer.  C-g cancels the build and launch."
  (let* ((result (jb-kotlin--command "intellij.java.resolveBuildCommand"
                                      (list :uri uri)))
         (command (append (lsp-get result :command) nil)))
    (if (not (eq t (lsp-get result :supported)))
        (message "Kotlin: no build step: %s"
                 (or (lsp-get result :reason) "not supported for this module"))
      (unless (and command (cl-every #'stringp command))
        (user-error "Kotlin server returned an invalid build command"))
      (let* ((default-directory
              (file-name-as-directory
               (or (lsp-get result :cwd) (lsp-workspace-root))))
             (buffer (generate-new-buffer "*Kotlin build*"))
             process)
        (with-current-buffer buffer
          (compilation-mode)
          (setq-local default-directory default-directory))
        (display-buffer buffer)
        (unwind-protect
            (progn
              (setq process (make-process
                             :name "Kotlin build" :buffer buffer
                             :command command :connection-type 'pipe
                             :noquery t))
              (while (process-live-p process)
                (accept-process-output process 0.1))
              (unless (and (eq (process-status process) 'exit)
                           (zerop (process-exit-status process)))
                (user-error "Kotlin build failed; see %s" (buffer-name buffer))))
          (when (and process (process-live-p process))
            (delete-process process)))))))

(defun jb-kotlin--resolve-jvm (config uri)
  "Resolve runtime paths for JVM CONFIG at URI."
  (when jb-kotlin-build-before-run (jb-kotlin--build uri))
  (let* ((overrides (make-hash-table :test 'equal))
         (_ (dolist (key '(:classPaths :modulePaths :moduleName :javaExec :vmArgs))
              (when (plist-member config key)
                (puthash (substring (symbol-name key) 1)
                         (plist-get config key) overrides))))
         (paths (jb-kotlin--command
                 "intellij.java.resolveLaunch"
                 (append (list :uri uri :overrides overrides)
                         (when (plist-get config :cwd)
                           (list :cwd (plist-get config :cwd)))))))
    (dolist (mapping '((:classpath . :classPaths)
                       (:modulePath . :modulePaths)
                       (:moduleContentPaths . :moduleContentPaths)))
      (setq config (plist-put config (cdr mapping)
                              (or (lsp-get paths (car mapping)) []))))
    (dolist (mapping '((:moduleName . :moduleName)
                       (:workingDirectory . :cwd)
                       (:javaExec . :javaExec)
                       (:vmArgs . :vmArgs)))
      (when-let* ((value (lsp-get paths (car mapping))))
        (setq config (plist-put config (cdr mapping) value))))
    config))

(defun jb-kotlin--resolve-gradle (config uri)
  "Resolve the build tool target for Gradle CONFIG at URI."
  (let ((target (jb-kotlin--command
                 "intellij.java.resolveBuildToolLaunch"
                 (list :uri uri :mainClass (plist-get config :mainClass))))
        (args (list :uri uri)))
    (unless (equal (lsp-get target :tool) "gradle")
      (user-error "This module cannot launch through Gradle; use intellij_jvm"))
    (when-let* ((module (lsp-get target :moduleName)))
      (setq args (plist-put args :moduleName module)))
    (dolist (mapping '((:projectPath . :projectPath)
                       (:sourceSet . :sourceSet) (:gradleArgs . :toolArgs)))
      (when (plist-member config (car mapping))
        (setq args (plist-put args (cdr mapping)
                              (plist-get config (car mapping))))))
    (setq config (plist-put config :buildToolTarget args))
    (plist-put config :classPaths (or (lsp-get target :scopeClassPaths) []))))

(defun jb-kotlin--debug-config (config)
  "Populate a dap-mode launch or attach CONFIG using Kotlin LSP."
  (let* ((workspace (jb-kotlin--workspace))
         (config (copy-tree config))
         (request (plist-get config :request)))
    (unless (member request '("launch" "attach"))
      (user-error "Kotlin DAP requires a launch or attach request"))
    (when (equal request "launch")
      (unless (plist-get config :mainClass)
        (user-error "Kotlin launch requires :mainClass"))
      (let ((uri (jb-kotlin--target-uri config)))
        (setq config
              (if (equal (plist-get config :type) "intellij_gradle")
                  (jb-kotlin--resolve-gradle config uri)
                (jb-kotlin--resolve-jvm config uri))))
      (unless (plist-get config :console)
        (setq config (plist-put config :console "internalConsole"))))
    (let ((port (jb-kotlin--command
                 "start_debug_server"
                 (lsp--path-to-uri (lsp--workspace-root workspace)))))
      (unless (and (integerp port) (< 0 port 65536))
        (user-error "Kotlin server returned an invalid DAP port: %S" port))
      (setq config (plist-put config :debugServer port))
      (plist-put config :host "localhost"))))

(defun jb-kotlin--run-main (args)
  "Run or debug the main class described by code lens ARGS."
  (let* ((main (lsp-get args :mainClass))
         (uri (lsp-get args :uri))
         (tool (when uri
                 (lsp-get (jb-kotlin--command
                           "intellij.java.resolveBuildToolLaunch"
                           (list :uri uri :mainClass main)) :tool))))
    (dap-debug (append
                (list :type (if (equal tool "gradle")
                                "intellij_gradle" "intellij_jvm")
                      :request "launch" :name main :mainClass main
                      :noDebug (if (eq t (lsp-get args :noDebug))
                                   t :json-false))
                (when uri (list :file (lsp--uri-to-path uri)))))))

(defun jb-kotlin--lens-action (command)
  "Handle a run/debug lens COMMAND locally."
  (let ((arguments (lsp-get command :arguments)))
    (unless (> (length arguments) 0)
      (user-error "Kotlin run lens has no arguments"))
    (jb-kotlin--run-main (elt arguments 0))))

;;;###autoload
(defun jb-kotlin-debug (main-class &optional no-debug)
  "Debug MAIN-CLASS in this Kotlin buffer; with prefix NO-DEBUG, run it."
  (interactive (list (read-string "Main class (e.g. com.example.MainKt): ")
                     current-prefix-arg))
  (unless buffer-file-name (user-error "Open a Kotlin source file first"))
  (let ((uri (lsp--path-to-uri buffer-file-name))
        (run (if no-debug t :json-false)))
    (jb-kotlin--run-main
     (if lsp-use-plists
         (list :mainClass main-class :uri uri :noDebug run)
       (lsp-ht ("mainClass" main-class) ("uri" uri) ("noDebug" run))))))

;;;###autoload
(defun jb-kotlin-attach (host port)
  "Attach to a JVM listening for JDWP on HOST and PORT."
  (interactive (list (read-string "JVM host: " "localhost")
                     (read-number "JDWP port: " 5005)))
  (dap-debug (list :type "intellij_jvm" :request "attach"
                   :name (format "Kotlin %s:%s" host port)
                   :hostName host :port port)))

(require 'jb-kotlin-reload)

(defun jb-kotlin--import-log (_workspace params)
  "Record project import PARAMS from WORKSPACE."
  (with-current-buffer (get-buffer-create "*Kotlin import*")
    (goto-char (point-max))
    (insert (or (lsp-get params :message) "") "\n")))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () jb-kotlin-server-command))
  :major-modes '(kotlin-mode kotlin-ts-mode)
  :priority 1 :server-id 'jb-kotlin
  :initialization-options #'jb-kotlin--initialization-options
  :uri-handlers (lsp-ht ("jar" #'jb-kotlin--source-uri)
                        ("jrt" #'jb-kotlin--source-uri)
                        ("command" #'jb-kotlin--command-uri))
  :action-handlers (lsp-ht ("intellij.jvm.runMain" #'jb-kotlin--lens-action)
                          ("jetbrains.navigateToLocation" #'jb-kotlin--navigate-action))
  :notification-handlers
  (lsp-ht ("intellij/importLog" #'jb-kotlin--import-log)
          ("intellij/workspaceImportStatus" #'ignore))))

(dolist (type '("intellij_jvm" "intellij_gradle" "intellij_debugger"))
  (dap-register-debug-provider type #'jb-kotlin--debug-config))

(dap-register-debug-template
 "Kotlin: JVM launch"
 '(:type "intellij_jvm" :request "launch" :name "Kotlin JVM"
   :mainClass "MainKt"))
(dap-register-debug-template
 "Kotlin: Gradle launch"
 '(:type "intellij_gradle" :request "launch" :name "Kotlin Gradle"
   :mainClass "MainKt"))
(dap-register-debug-template
 "Kotlin: Attach"
 '(:type "intellij_jvm" :request "attach" :name "Kotlin attach"
   :hostName "localhost" :port 5005))

(provide 'jb-kotlin-lsp-dap)
;;; jb-kotlin-lsp-dap.el ends here

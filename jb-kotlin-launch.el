;;; jb-kotlin-launch.el --- Project launch configurations -*- lexical-binding: t; -*-

;;; Commentary:
;; Create editable launch.json configurations consumed by dap-mode.

;;; Code:
(require 'json)
(require 'dap-mode)
(require 'dap-utils)
(require 'jb-kotlin-reload)

(declare-function jb-kotlin--workspace "jb-kotlin-lsp-dap")
(declare-function jb-kotlin--command "jb-kotlin-lsp-dap" (command &rest arguments))

(defun jb-kotlin--launch-root ()
  "Return the current Kotlin workspace root without registering a project."
  (lsp--workspace-root
   (or (jb-kotlin--file-workspace (or buffer-file-name default-directory))
       (jb-kotlin--workspace))))

(defun jb-kotlin--launch-json ()
  "Parse the current JSONC buffer without changing its text."
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (condition-case err
        (with-temp-buffer
          (insert text) (goto-char (point-min)) (dap-utils-sanitize-json)
          (goto-char (point-min))
          (prog1 (json-parse-buffer :object-type 'hash-table :array-type 'array
                                   :null-object nil :false-object :json-false)
            (skip-chars-forward " \t\r\n")
            (unless (eobp) (error "Unexpected text after JSON object"))))
      (error (user-error "Invalid launch.json: %s" (error-message-string err))))))

(defun jb-kotlin--launch-configurations (json)
  "Validate JSON's configuration container and return its vector."
  (unless (and (hash-table-p json) (vectorp (gethash "configurations" json)))
    (user-error "launch.json must contain a configurations array"))
  (gethash "configurations" json))

(defun jb-kotlin--launch-array-end ()
  "Find the end of the top-level configurations array in this JSONC buffer.
Return (POSITION . NEED-COMMA), ignoring strings and comments."
  (save-excursion
    (goto-char (point-min))
    (let ((depth 0) key found last)
      (catch 'end
        (while (re-search-forward
                "\"\\(?:\\\\.\\|[^\"\\\\]\\)*\"\\|//[^\n]*\\|/\\*\\(?:[^*]\\|\\*+[^*/]\\)*\\*+/\\|[][{}:,]" nil t)
          (let ((token (match-string-no-properties 0)))
            (unless (string-prefix-p "/" token)
              (cond
               ((member token '("{" "["))
                (when (and (= depth 1) key (equal token "[")) (setq found t))
                (cl-incf depth))
               ((member token '("}" "]"))
                (when (and found (= depth 2) (equal token "]"))
                  (throw 'end (cons (match-beginning 0) (not (member last '("[" ","))))))
                (cl-decf depth))
               ((and (= depth 1) (string-prefix-p "\"" token))
                (setq key (equal (json-parse-string token) "configurations"))))
              (setq last token))))
        (user-error "Cannot locate the configurations array")))))

(defun jb-kotlin--insert-launch (config)
  "Append CONFIG to this launch.json buffer, preserving existing text."
  (let* ((entries (jb-kotlin--launch-configurations (jb-kotlin--launch-json)))
         (name (plist-get config :name))
         (position (jb-kotlin--launch-array-end))
         (json-encoding-pretty-print t)
         (text (json-encode config)))
    (when (seq-some (lambda (entry) (and (hash-table-p entry) (equal name (gethash "name" entry)))) entries)
      (user-error "A configuration named %s already exists" name))
    (atomic-change-group
      (goto-char (car position))
      (insert (if (cdr position) ",\n" "\n")
              (mapconcat (lambda (line) (concat "    " line)) (split-string text "\n") "\n")
              "\n  "))))

;;;###autoload
(defun jb-kotlin-open-launch-file ()
  "Open this workspace's .vscode/launch.json, creating an unsaved draft if absent."
  (interactive)
  (let ((file (expand-file-name ".vscode/launch.json" (jb-kotlin--launch-root))))
    (make-directory (file-name-directory file) t)
    (find-file file)
    (when (and (not (file-exists-p file)) (= (buffer-size) 0))
      (insert "{\n  \"version\": \"0.2.0\",\n  \"configurations\": []\n}\n"))))

(defun jb-kotlin--main-classes ()
  "Return main classes advertised by code lenses in the current source file."
  (when (and buffer-file-name (lsp-workspaces))
    (let (classes)
      (mapc (lambda (lens)
              (let* ((command (lsp-get lens :command))
                     (args (lsp-get command :arguments)))
                (when (and (equal (lsp-get command :command) "intellij.jvm.runMain")
                           (> (length args) 0))
                  (when-let* ((main (lsp-get (elt args 0) :mainClass)))
                    (cl-pushnew main classes :test #'equal)))))
            (lsp-request "textDocument/codeLens" (list :textDocument (lsp--text-document-identifier))))
      (nreverse classes))))

;;;###autoload
(defun jb-kotlin-add-launch-configuration ()
  "Add a launch or attach entry to .vscode/launch.json for review and saving."
  (interactive)
  (let* ((root (jb-kotlin--launch-root))
         (source buffer-file-name)
         (kind (completing-read "Configuration: " '("JVM launch" "Gradle launch" "Attach") nil t))
         (attach (equal kind "Attach"))
         (main (unless attach
                 (completing-read "Main class: "
                                  (condition-case err (jb-kotlin--main-classes)
                                    (error (message "Main class suggestions unavailable: %s"
                                                    (error-message-string err)) nil))
                                  nil nil)))
         (name (read-string "Configuration name: " (if attach "Kotlin: Attach" (concat "Kotlin: " main))))
         (config (list :type (if (equal kind "Gradle launch") "intellij_gradle" "intellij_jvm")
                       :request (if attach "attach" "launch") :name name)))
    (when (string-empty-p (string-trim name)) (user-error "Configuration name is required"))
    (if attach
        (let ((host (read-string "JVM host: " "localhost"))
              (port (read-number "JDWP port: " 5005)))
          (unless (and (integerp port) (< 0 port 65536)) (user-error "Invalid JDWP port"))
          (setq config (append config (list :hostName host :port port))))
      (when (string-empty-p (string-trim main)) (user-error "Main class is required"))
      (setq config (append config (list :mainClass main :cwd "${workspaceFolder}"
                                        :console (completing-read "Console: " '("integratedTerminal" "internalConsole") nil t)
                                        :args [])))
      (when (and source (string-match-p "\\.\\(?:kt\\|java\\)\\'" source)
                 (file-in-directory-p source root))
        (setq config (append config (list :file (concat "${workspaceFolder}/" (file-relative-name source root)))))))
    (jb-kotlin-open-launch-file)
    (jb-kotlin--insert-launch config)
    (message "Configuration added; review and save with C-x C-s, then run M-x dap-debug")))

;;;###autoload
(defun jb-kotlin-validate-launch-file ()
  "Validate Kotlin entries in the current workspace's launch.json draft."
  (interactive)
  (unless (equal (file-name-nondirectory (or buffer-file-name "")) "launch.json")
    (jb-kotlin-open-launch-file))
  (let ((entries (jb-kotlin--launch-configurations (jb-kotlin--launch-json))) (count 0) names)
    (mapc
     (lambda (entry)
       (unless (hash-table-p entry) (user-error "Each configuration must be an object"))
       (when (member (gethash "type" entry) '("intellij_jvm" "intellij_gradle" "intellij_debugger"))
         (cl-incf count)
         (let ((name (gethash "name" entry)) (request (gethash "request" entry)))
           (unless (and (stringp name) (not (string-empty-p (string-trim name))))
             (user-error "Kotlin configuration needs a name"))
           (when (member name names) (user-error "Duplicate configuration name: %s" name))
           (push name names)
           (pcase request
             ("launch"
              (unless (and (stringp (gethash "mainClass" entry))
                           (not (string-empty-p (gethash "mainClass" entry))))
                (user-error "%s needs mainClass" name))
              (when-let* ((console (gethash "console" entry)))
                (unless (member console '("internalConsole" "integratedTerminal" "externalTerminal"))
                  (user-error "%s has an unknown console" name))))
             ("attach"
              (let ((port (gethash "port" entry)))
                (unless (and (integerp port) (< 0 port 65536))
                  (user-error "%s needs a JDWP port between 1 and 65535" name))))
             (_ (user-error "%s needs request launch or attach" name)))
           (dolist (key '("args" "vmArgs" "classPaths" "modulePaths" "gradleArgs"))
             (let ((value (gethash key entry 'absent)))
               (unless (or (eq value 'absent) (and (vectorp value) (seq-every-p #'stringp value)))
                 (user-error "%s: %s must be an array of strings" name key))))))) entries)
    (message "%d Kotlin configurations valid" count)
    count))

(provide 'jb-kotlin-launch)
;;; jb-kotlin-launch.el ends here

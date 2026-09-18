;;; jb-kotlin-launch-test.el --- Launch file and console tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'jb-kotlin-lsp-dap)

(ert-deftest jb-kotlin-launch-jsonc-preserves-comments-and-other-entries ()
  (with-temp-buffer
    (insert "{\n // keep this\n \"version\": \"0.2.0\",\n \"configurations\": [\n {\"name\":\"Other\",\"type\":\"python\",\"args\":[\"brackets ] // /*\"]}, // trailing comma\n ],\n \"compounds\": [],\n}\n")
    (jb-kotlin--insert-launch '(:name "Kotlin" :type "intellij_jvm" :request "launch" :mainClass "demo.MainKt" :args []))
    (should (string-match-p "// keep this" (buffer-string)))
    (should (string-match-p "// trailing comma" (buffer-string)))
    (let* ((json (jb-kotlin--launch-json)) (entries (gethash "configurations" json)))
      (should (= 2 (length entries)))
      (should (equal "Other" (gethash "name" (aref entries 0))))
      (should (equal [] (gethash "args" (aref entries 1))))
      (should (equal [] (gethash "compounds" json))))))

(ert-deftest jb-kotlin-launch-appends-without-trailing-comma ()
  (dolist (initial '("{\"configurations\": []}" "{\"configurations\": [{\"name\":\"Old\"}]}"))
    (with-temp-buffer
      (insert initial)
      (jb-kotlin--insert-launch '(:name "New" :type "intellij_jvm" :request "attach" :port 5005))
      (should (equal "New" (gethash "name" (car (last (append (gethash "configurations" (jb-kotlin--launch-json)) nil)))))))))

(ert-deftest jb-kotlin-launch-rejects-invalid-file-or-duplicate-without-edit ()
  (dolist (initial '("{invalid}" "{\"configurations\": []} trailing" "{\"configurations\": {}}" "{\"configurations\": [{\"name\":\"Existing\"}]}"))
    (with-temp-buffer
      (insert initial)
      (should-error (jb-kotlin--insert-launch '(:name "Existing")) :type 'user-error)
      (should (equal initial (buffer-string))))))

(ert-deftest jb-kotlin-launch-validation-checks-ports-and-argument-arrays ()
  (dolist (entry '((:type "intellij_jvm" :name "Bad" :request "attach" :port 0)
                   (:type "intellij_jvm" :name "Bad" :request "launch")
                   (:type "intellij_jvm" :name "Bad" :request "launch" :mainClass "MainKt" :args "hello")
                   (:type "intellij_jvm" :name "Bad" :request "launch" :mainClass "MainKt" :console "wrong")))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/launch.json")
      (insert (json-encode (list :configurations (vector entry))))
      (should-error (jb-kotlin-validate-launch-file) :type 'user-error)))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/launch.json")
    (insert "{\"configurations\":[{\"type\":\"intellij_jvm\",\"name\":\"Attach\",\"request\":\"attach\",\"port\":5005}]}")
    (should (= 1 (jb-kotlin-validate-launch-file)))))

(ert-deftest jb-kotlin-launch-main-suggestions-deduplicate-run-debug-lenses ()
  (jb-kotlin-test--workspace
    (let* ((buffer-file-name "/tmp/project/Main.kt")
           (arg (jb-kotlin-test--object ("mainClass" "demo.MainKt")))
           (command (jb-kotlin-test--object ("command" "intellij.jvm.runMain") ("arguments" (vector arg))))
           (lens (jb-kotlin-test--object ("command" command))))
      (cl-letf (((symbol-function 'lsp-request)
                 (lambda (method _params)
                   (should (equal method "textDocument/codeLens"))
                   (vector lens lens))))
        (should (equal '("demo.MainKt") (jb-kotlin--main-classes)))))))

(ert-deftest jb-kotlin-terminal-preserves-argv-env-and-input ()
  (let* ((session (make-dap--debug-session :name "Test" :launch-args '(:type "intellij_jvm")))
         (jb-kotlin--terminal-processes (make-hash-table :test 'eq))
         (process-environment (copy-sequence process-environment))
         response process buffer)
    (setenv "JB_DROP_ME" "present")
    (unwind-protect
        (cl-letf (((symbol-function 'dap--send-message) (lambda (message &rest _) (setq response message)))
                  ((symbol-function 'display-buffer) #'ignore))
          (jb-kotlin--dap-terminal
           (lambda (&rest _) (ert-fail "Wrong terminal handler")) session
           (ht ("seq" 9) ("arguments" (ht ("kind" "integrated") ("cwd" temporary-file-directory)
                                          ("env" (ht ("JB_VALUE" "space $literal") ("JB_DROP_ME" nil)))
                                          ("args" (vector (executable-find "python3") "-u" "-c"
                                                          "import os,sys; print(repr(sys.argv[1:])); print(os.getenv('JB_VALUE')); print(os.getenv('JB_DROP_ME')); print('input='+input())"
                                                          "a b" "$(literal)"))))))
          (should (plist-get response :success))
          (setq process (car (gethash session jb-kotlin--terminal-processes)) buffer (process-buffer process))
          (should (= (process-id process) (plist-get (plist-get response :body) :processId)))
          (process-send-string process "hello\n")
          (let ((deadline (+ (float-time) 5)))
            (while (and (process-live-p process) (< (float-time) deadline)) (accept-process-output process 0.1)))
          (with-current-buffer buffer
            (should (derived-mode-p 'comint-mode))
            (should (string-match-p (regexp-quote "['a b', '$(literal)']") (buffer-string)))
            (should (string-match-p (regexp-quote "space $literal") (buffer-string)))
            (should (string-match-p "None" (buffer-string)))
            (should (string-match-p "input=hello" (buffer-string))))
          (should (equal "present" (getenv "JB_DROP_ME"))))
      (jb-kotlin--terminal-cleanup session)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest jb-kotlin-terminal-leaves-other-adapters-alone ()
  (let ((session (make-dap--debug-session :launch-args '(:type "python"))))
    (should (eq 'delegated (jb-kotlin--dap-terminal (lambda (&rest _) 'delegated) session nil)))))

(ert-deftest jb-kotlin-terminal-rejects-shell-arguments-and-unknown-kind ()
  (let ((session (make-dap--debug-session :launch-args '(:type "intellij_jvm"))) response)
    (dolist (kind '("integrated" "unknown"))
      (cl-letf (((symbol-function 'dap--send-message) (lambda (message &rest _) (setq response message)))
                ((symbol-function 'make-comint-in-buffer) (lambda (&rest _) (ert-fail "Unexpected process"))))
        (jb-kotlin--dap-terminal
         #'ignore session
         (ht ("seq" 1) ("arguments" (ht ("kind" kind) ("cwd" temporary-file-directory)
                                        ("args" ["echo" "$(unsafe)"]) ("argsCanBeInterpretedByShell" t)))))
        (should-not (eq t (plist-get response :success)))))))

(ert-deftest jb-kotlin-terminal-cleans-only-own-session ()
  (let* ((jb-kotlin--terminal-processes (make-hash-table :test 'eq))
         (first (make-pipe-process :name "jb-terminal-first" :noquery t))
         (second (make-pipe-process :name "jb-terminal-second" :noquery t)))
    (unwind-protect
        (progn
          (puthash 'first (list first) jb-kotlin--terminal-processes)
          (puthash 'second (list second) jb-kotlin--terminal-processes)
          (jb-kotlin--terminal-cleanup 'first)
          (should-not (process-live-p first))
          (should-not (gethash 'first jb-kotlin--terminal-processes))
          (should (process-live-p second)))
      (delete-process first)
      (delete-process second))))

(ert-deftest jb-kotlin-launch-wizard-creates-portable-unsaved-draft ()
  (let* ((root (make-temp-file "jb-launch-wizard-" t))
         (source (expand-file-name "Main.kt" root))
         (launch (expand-file-name ".vscode/launch.json" root))
         (answers '("JVM launch" "demo.MainKt" "integratedTerminal")))
    (unwind-protect
        (save-window-excursion
          (with-temp-buffer
            (setq buffer-file-name source)
            (cl-letf (((symbol-function 'jb-kotlin--launch-root) (lambda () root))
                      ((symbol-function 'jb-kotlin--main-classes) (lambda () '("demo.MainKt")))
                      ((symbol-function 'completing-read) (lambda (&rest _) (pop answers)))
                      ((symbol-function 'read-string) (lambda (&rest _) "Demo")))
              (jb-kotlin-add-launch-configuration)))
          (with-current-buffer (get-file-buffer launch)
            (should (buffer-modified-p))
            (should-not (file-exists-p launch))
            (should (= 1 (jb-kotlin-validate-launch-file)))
            (let ((entry (aref (gethash "configurations" (jb-kotlin--launch-json)) 0)))
              (should (equal "${workspaceFolder}/Main.kt" (gethash "file" entry)))
              (should (equal "integratedTerminal" (gethash "console" entry))))))
      (when-let* ((buffer (get-file-buffer launch)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest jb-kotlin-debug-config-from-launch-file-finds-own-workspace ()
  (let* ((lsp--buffer-workspaces nil) (lsp--cur-workspace nil)
         (buffer-file-name "/tmp/project/.vscode/launch.json")
         (workspace (make-lsp--workspace :root "/tmp/project")))
    (cl-letf (((symbol-function 'jb-kotlin--file-workspace)
               (lambda (file) (should (equal file buffer-file-name)) workspace)))
      (should (eq workspace (jb-kotlin--workspace))))))

(provide 'jb-kotlin-launch-test)

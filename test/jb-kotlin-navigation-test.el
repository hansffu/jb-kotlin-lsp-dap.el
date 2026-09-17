;;; jb-kotlin-navigation-test.el --- Source navigation tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'jb-kotlin-lsp-dap)

(defmacro jb-kotlin-test--sources (&rest body)
  "Evaluate BODY with private source caches and a test workspace."
  (declare (indent 0))
  `(jb-kotlin-test--workspace
     (let ((jb-kotlin--source-caches (make-hash-table :test 'eq))
           (jb-kotlin--source-directories nil))
       (unwind-protect
           (cl-letf (((symbol-function 'lsp--open-in-workspace)
                      (lambda (w)
                        (should (eq workspace w))
                        (should (string-match-p "\\`\\(?:jar\\|jrt\\):" (lsp--buffer-uri))))))
             ,@body)
         (dolist (buf (buffer-list))
           (when (and (buffer-local-value 'buffer-file-name buf)
                      (cl-some (lambda (dir)
                                 (file-in-directory-p
                                  (buffer-local-value 'buffer-file-name buf) dir))
                               jb-kotlin--source-directories))
             (kill-buffer buf)))
         (jb-kotlin--clear-source-caches)))))

(ert-deftest jb-kotlin-source-uri-caches-and-reopens-original-document ()
  (jb-kotlin-test--sources
    (let ((calls 0) (uri "jar:///tmp/library%20one.jar!/demo/Greeter.class")
          (java-mode-hook (list (lambda () (ert-fail "Do not run user mode hooks")))))
      (cl-letf (((symbol-function 'jb-kotlin--command)
                 (lambda (command &rest args)
                   (cl-incf calls)
                   (should (equal command "decompile"))
                   (should (equal args (list uri)))
                   (jb-kotlin-test--object ("code" "package demo;\nclass Greeter {}\n")
                                           ("language" "java")))))
        (let* ((file (lsp--uri-to-path uri)) (buf (get-file-buffer file)))
          (should (file-readable-p file))
          (with-current-buffer buf
            (should buffer-read-only)
            (should (eq major-mode 'java-mode))
            (should (equal uri (lsp--buffer-uri)))
            (should (eq workspace (jb-kotlin--workspace)))
            (should-error (insert "oops") :type 'buffer-read-only))
          (should (equal file (lsp--uri-to-path uri)))
          (kill-buffer buf)
          (should (equal file (lsp--uri-to-path uri)))
          (should (buffer-live-p (get-file-buffer file)))
          (should (= calls 1)))))))

(ert-deftest jb-kotlin-source-cache-isolates-workspaces-and-uris ()
  (jb-kotlin-test--sources
    (cl-letf (((symbol-function 'jb-kotlin--command)
               (lambda (&rest _) (jb-kotlin-test--object ("code" "class A {}") ("language" "java")))))
      (let* ((uri "jrt:///jdk!/java.base/demo/A.class")
             (first (jb-kotlin--source-uri uri))
             (second (jb-kotlin--source-uri "jar:///other.jar!/demo/A.class"))
             (workspace (make-lsp--workspace :client (gethash 'jb-kotlin lsp-clients)
                                             :root "/tmp/other-project"))
             (lsp--buffer-workspaces (list workspace)))
        (should-not (equal first second))
        (cl-letf (((symbol-function 'lsp--open-in-workspace) #'ignore))
          (should-not (equal first (jb-kotlin--source-uri uri))))))))

(ert-deftest jb-kotlin-source-failure-can-be-retried ()
  (jb-kotlin-test--sources
    (let ((uri "jar:///library.jar!/A.class"))
      (cl-letf (((symbol-function 'jb-kotlin--command) (lambda (&rest _) nil)))
        (should-error (jb-kotlin--source-uri uri) :type 'user-error)
        (should-not jb-kotlin--source-directories))
      (cl-letf (((symbol-function 'jb-kotlin--command)
                 (lambda (&rest _) (jb-kotlin-test--object ("code" "class A") ("language" "kotlin")))))
        (should (file-readable-p (jb-kotlin--source-uri uri)))))))

(ert-deftest jb-kotlin-navigation-command-and-hover-link-use-lsp-coordinates ()
  (jb-kotlin-test--sources
    (let ((uri "jar:///library%20%C3%A6.jar!/A.class"))
      (cl-letf (((symbol-function 'jb-kotlin--command)
                 (lambda (&rest _) (jb-kotlin-test--object ("code" "// 😀 café\nclass A {}\n") ("language" "java")))))
        (save-window-excursion
          (lsp--execute-command
           (jb-kotlin-test--object ("command" "jetbrains.navigateToLocation")
                                   ("arguments" (vector uri 0 6))))
          ;; UTF-16 column 6 is after the space following the emoji.
          (should (looking-at "café"))
          (lsp--document-link-handle-target
           (concat "command:jetbrains.navigateToLocation?"
                   (url-hexify-string (json-encode (vector uri 1 6)))))
          (should (looking-at "A {}"))
          (should (equal uri (lsp--buffer-uri))))))))

(ert-deftest jb-kotlin-navigation-rejects-invalid-and-unrelated-commands ()
  (dolist (uri '("command:workbench.action.quit?[]"
                 "command:jetbrains.navigateToLocation?{}"
                 "command:jetbrains.navigateToLocation?invalid"
                 "command:jetbrains.navigateToLocation?[\"https://example.com\",0,0]"
                 "command:jetbrains.navigateToLocation?[\"file:///tmp/a\",-1,0]"))
    (should-error (jb-kotlin--command-uri uri) :type 'user-error)))

(ert-deftest jb-kotlin-navigation-file-link-preserves-unicode-path ()
  (let ((file (make-temp-file "jb-kotlin-æ-" nil ".kt" "// 😀 café\nfun main() {}\n")))
    (unwind-protect
        (save-window-excursion
          (jb-kotlin--command-uri
           (concat "command:jetbrains.navigateToLocation?"
                   (url-hexify-string (json-encode (vector (lsp--path-to-uri file) 0 6)))))
          (should (equal (file-truename file) (file-truename buffer-file-name)))
          (should (looking-at "café")))
      (when-let* ((buffer (get-file-buffer file))) (kill-buffer buffer))
      (delete-file file))))

(ert-deftest jb-kotlin-navigation-help-buffer-asks-for-workspace ()
  (jb-kotlin-test--workspace
    (let* ((other (make-lsp--workspace :client (gethash 'jb-kotlin lsp-clients)
                                      :root "/tmp/other" :status 'initialized))
           (lsp--buffer-workspaces nil))
      (setf (lsp--workspace-status workspace) 'initialized)
      (cl-letf (((symbol-function 'lsp--session-workspaces) (lambda (_) (list workspace other)))
                ((symbol-function 'completing-read) (lambda (&rest _) "/tmp/other")))
        (should (eq other (jb-kotlin--navigation-workspace)))))))

(provide 'jb-kotlin-navigation-test)
;;; jb-kotlin-navigation-test.el ends here

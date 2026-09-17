;;; jb-kotlin-reload-test.el --- Build reload tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'jb-kotlin-lsp-dap)

(defmacro jb-kotlin-test--reload (&rest body)
  "Run BODY with two private workspaces, controlled timers and requests."
  (declare (indent 0))
  `(let* ((root (make-temp-file "jb-reload-test-" t))
          (nested (expand-file-name "nested" root))
          (workspace (make-lsp--workspace :root root :status 'initialized
                                          :client (gethash 'jb-kotlin lsp-clients)))
          (other (make-lsp--workspace :root nested :status 'initialized
                                      :client (gethash 'jb-kotlin lsp-clients)))
          (jb-kotlin--reload-states (make-hash-table :test 'eq))
          (jb-kotlin-reload-on-save 'always)
          requests timers cancelled)
     (make-directory nested)
     (unwind-protect
         (cl-letf (((symbol-function 'lsp--session-workspaces)
                    (lambda (_) (list workspace other)))
                   ((symbol-function 'lsp-workspace-folders) (lambda (_) nil))
                   ((symbol-function 'run-with-idle-timer)
                    (lambda (_delay _repeat callback &rest args)
                      (let ((timer (cons callback args))) (push timer timers) timer)))
                   ((symbol-function 'cancel-timer) (lambda (timer) (push timer cancelled)))
                   ((symbol-function 'lsp-request-async)
                    (lambda (method params callback &rest keywords)
                      (should (equal method "intellij/reloadWorkspace"))
                      (should (eq (plist-get keywords :mode) 'detached))
                      (push (list :workspace lsp--cur-workspace :params params
                                  :success callback :error (plist-get keywords :error-handler))
                            requests))))
           ,@body)
       (delete-directory root t))))

(defun jb-kotlin-test--save-build (root file)
  "Run the real after-save hook for FILE inside ROOT without LSP."
  (with-temp-buffer
    (setq buffer-file-name (expand-file-name file root))
    (run-hooks 'after-save-hook)))

(defun jb-kotlin-test--flush (workspace)
  "Fire WORKSPACE's currently scheduled timer."
  (let ((timer (jb-kotlin--reload-state-timer
                (gethash workspace jb-kotlin--reload-states))))
    (should timer)
    (apply (car timer) (cdr timer))))

(ert-deftest jb-kotlin-reload-build-descriptors-exclude-server-owned-files ()
  (dolist (name '("pom.xml" "build.gradle" "build.gradle.kts" "settings.gradle"
                  "settings.gradle.kts" "BUILD" "BUILD.bazel" "MODULE.bazel"
                  "WORKSPACE" "WORKSPACE.bazel" ".bazelproject" "rules.bzl"))
    (should (jb-kotlin--build-file-p (concat "/tmp/project/" name))))
  (dolist (name '(nil "/tmp/workspace.json" "/tmp/Main.kt" "/tmp/pom.xml.bak"
                  "/tmp/build.gradle.kts~" "/ssh:host:/tmp/pom.xml"))
    (should-not (jb-kotlin--build-file-p name))))

(ert-deftest jb-kotlin-reload-coalesces-saves-and-sends-current-options ()
  (jb-kotlin-test--reload
    (let ((jb-kotlin-default-sdk "/first-jdk"))
      (jb-kotlin-test--save-build root "build.gradle.kts"))
    (let ((jb-kotlin-default-sdk "/second-jdk"))
      (jb-kotlin-test--save-build root "settings.gradle.kts"))
    (should (= (length cancelled) 1))
    (should-not requests)
    (jb-kotlin-test--flush workspace)
    (should (= (length requests) 1))
    (should (eq workspace (plist-get (car requests) :workspace)))
    (let ((options (plist-get (plist-get (car requests) :params) :initializationOptions)))
      (should (equal "/second-jdk" (plist-get options :defaultSdk)))
      (should (eq t (plist-get options :runMainCodeLens))))
    (funcall (plist-get (car requests) :success) nil)
    (should-not (jb-kotlin--reload-state-timer (gethash workspace jb-kotlin--reload-states)))))

(ert-deftest jb-kotlin-reload-selects-nearest-workspace-and-ignores-unowned-files ()
  (jb-kotlin-test--reload
    (jb-kotlin-test--save-build root "nested/pom.xml")
    (should-not (gethash workspace jb-kotlin--reload-states))
    (jb-kotlin-test--flush other)
    (should (eq other (plist-get (car requests) :workspace)))
    (jb-kotlin-test--save-build "/tmp" "pom.xml")
    (jb-kotlin-test--save-build root "workspace.json")
    (setf (lsp--workspace-status workspace) 'shutdown)
    (jb-kotlin-test--save-build root "build.gradle")
    (should (= 1 (hash-table-count jb-kotlin--reload-states)))))

(ert-deftest jb-kotlin-reload-prompt-decline-and-quit-allow-later-retry ()
  (jb-kotlin-test--reload
    (let ((jb-kotlin-reload-on-save 'prompt))
      (dolist (answer '(no quit yes))
        (jb-kotlin-test--save-build root "pom.xml")
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (_) (pcase answer ('no nil) ('quit (signal 'quit nil)) (_ t)))))
          (condition-case nil (jb-kotlin-test--flush workspace) (quit nil)))
        (unless (eq answer 'yes)
          (should-not requests)
          (should-not (jb-kotlin--reload-state-busy
                       (gethash workspace jb-kotlin--reload-states)))))
      (should (= 1 (length requests))))))

(ert-deftest jb-kotlin-reload-saves-during-prompt-are-covered-by-one-request ()
  (jb-kotlin-test--reload
    (let ((jb-kotlin-reload-on-save 'prompt))
      (jb-kotlin-test--save-build root "pom.xml")
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_) (jb-kotlin-test--save-build root "pom.xml") t)))
        (jb-kotlin-test--flush workspace))
      (funcall (plist-get (car requests) :success) nil)
      (should (= 1 (length requests)))
      (should-not (jb-kotlin--reload-state-timer (gethash workspace jb-kotlin--reload-states))))))

(ert-deftest jb-kotlin-reload-saves-during-import-coalesce-without-blocking-other-projects ()
  (jb-kotlin-test--reload
    (jb-kotlin-test--save-build root "pom.xml")
    (jb-kotlin-test--flush workspace)
    (let ((first (car requests)))
      (jb-kotlin-test--save-build root "pom.xml")
      (jb-kotlin-test--save-build root "build.gradle")
      (jb-kotlin-test--save-build nested "pom.xml")
      (jb-kotlin-test--flush other)
      (should (= 2 (length requests)))
      (funcall (plist-get first :success) nil)
      (jb-kotlin-test--flush workspace)
      (should (= 3 (length requests)))
      (should (eq workspace (plist-get (car requests) :workspace))))))

(ert-deftest jb-kotlin-reload-manual-works-in-build-buffer-and-supersedes-timer ()
  (jb-kotlin-test--reload
    (let ((jb-kotlin-reload-on-save 'never))
      (jb-kotlin-test--save-build root "pom.xml")
      (jb-kotlin-test--flush workspace)
      (should-not requests)
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "pom.xml" root))
        (jb-kotlin-reload-workspace))
      (should (= 1 (length requests)))
      (should-not (jb-kotlin--reload-state-timer (gethash workspace jb-kotlin--reload-states))))))

(ert-deftest jb-kotlin-reload-errors-allow-retry-without-retry-loop ()
  (jb-kotlin-test--reload
    (jb-kotlin-test--save-build root "pom.xml")
    (jb-kotlin-test--flush workspace)
    (funcall (plist-get (car requests) :error) '(:message "Import failed"))
    (should-not (jb-kotlin--reload-state-timer (gethash workspace jb-kotlin--reload-states)))
    (jb-kotlin-test--save-build root "pom.xml")
    (jb-kotlin-test--flush workspace)
    (should (= 2 (length requests)))))

(ert-deftest jb-kotlin-reload-manual-cancels-queued-automatic-request ()
  (jb-kotlin-test--reload
    (jb-kotlin-test--save-build root "pom.xml")
    (let ((timer (car timers)))
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "pom.xml" root))
        (jb-kotlin-reload-workspace))
      (should (memq timer cancelled))
      (funcall (plist-get (car requests) :success) nil)
      (apply (car timer) (cdr timer))
      (should (= 1 (length requests))))))

(ert-deftest jb-kotlin-reload-send-error-releases-workspace ()
  (jb-kotlin-test--reload
    (jb-kotlin-test--save-build root "pom.xml")
    (cl-letf (((symbol-function 'lsp-request-async) (lambda (&rest _) (error "Connection lost"))))
      (jb-kotlin-test--flush workspace))
    (should-not (jb-kotlin--reload-state-busy (gethash workspace jb-kotlin--reload-states)))
    (jb-kotlin-test--save-build root "pom.xml")
    (jb-kotlin-test--flush workspace)
    (should (= 1 (length requests)))))

(ert-deftest jb-kotlin-reload-disconnect-during-prompt-does-not-send-request ()
  (jb-kotlin-test--reload
    (let ((jb-kotlin-reload-on-save 'prompt))
      (jb-kotlin-test--save-build root "pom.xml")
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_) (jb-kotlin--reload-forget workspace) t)))
        (jb-kotlin-test--flush workspace))
      (should-not requests)
      (should-not (gethash workspace jb-kotlin--reload-states)))))

(ert-deftest jb-kotlin-reload-disconnect-discards-timers-and-late-callbacks ()
  (jb-kotlin-test--reload
    (jb-kotlin-test--save-build root "pom.xml")
    (jb-kotlin-test--flush workspace)
    (jb-kotlin-test--save-build root "pom.xml")
    (run-hook-with-args 'lsp-after-uninitialized-functions workspace)
    (funcall (plist-get (car requests) :success) nil)
    (should-not (gethash workspace jb-kotlin--reload-states))
    (jb-kotlin-test--save-build nested "pom.xml")
    (let ((stale (car timers)))
      (run-hook-with-args 'lsp-after-uninitialized-functions other)
      (should (memq stale cancelled))
      (apply (car stale) (cdr stale))
      (should (= 1 (length requests))))))

(provide 'jb-kotlin-reload-test)
;;; jb-kotlin-reload-test.el ends here

;;; jb-kotlin-import-test.el --- Import status tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'jb-kotlin-lsp-dap)

(defun jb-kotlin-test--import-event (workspace flag text)
  "Deliver an import log with FLAG and TEXT to WORKSPACE in wire format."
  (jb-kotlin--import-log
   workspace (if lsp-use-plists (list flag t :message text)
               (lsp-ht ((substring (symbol-name flag) 1) t) ("message" text)))))

(defun jb-kotlin-test--blocked (dismissed)
  "Construct an ambiguous build-tool status with DISMISSED flag."
  (jb-kotlin-test--object
   ("blockedFolders"
    (vector (jb-kotlin-test--object
             ("folderUri" "file:///project") ("reason" "ambiguousBuildSystem")
             ("candidates" ["gradle" "maven"]) ("dismissed" dismissed))))))

(ert-deftest jb-kotlin-import-handlers-are-registered ()
  (let ((handlers (lsp--client-notification-handlers (gethash 'jb-kotlin lsp-clients))))
    (should (eq #'jb-kotlin--import-log (gethash "intellij/importLog" handlers)))
    (should (eq #'jb-kotlin--import-status-notification (gethash "intellij/workspaceImportStatus" handlers)))
    (should (eq #'jb-kotlin--import-state-notification (gethash "intellij/workspaceImportState" handlers)))))

(ert-deftest jb-kotlin-import-keeps-two-workspaces-and-their-logs-independent ()
  (jb-kotlin-test--reload
    (jb-kotlin-test--import-event workspace :started "first started")
    (jb-kotlin-test--import-event other :succeeded "second succeeded")
    (jb-kotlin-test--import-event workspace :failed "first failed")
    (let ((first (jb-kotlin--import-state workspace)) (second (jb-kotlin--import-state other)))
      (should (equal "Import failed" (jb-kotlin--import-label first)))
      (should (equal "Ready" (jb-kotlin--import-label second)))
      (with-current-buffer (jb-kotlin--import-state-log first)
        (should buffer-read-only)
        (should (string-match-p "first failed" (buffer-string)))
        (should-not (string-match-p "second" (buffer-string))))
      (let ((lsp--buffer-workspaces (list workspace)))
        (should (string-match-p "Import failed" (jb-kotlin--import-lighter))))
      (let ((lsp--buffer-workspaces (list other)))
        (should (string-match-p "Ready" (jb-kotlin--import-lighter)))))))

(ert-deftest jb-kotlin-import-json-false-and-ordinary-output-do-not-change-outcome ()
  (jb-kotlin-test--reload
    (jb-kotlin-test--import-event workspace :started "starting")
    (jb-kotlin--import-log workspace (jb-kotlin-test--object
                                    ("message" "output") ("failed" :json-false)
                                    ("succeeded" :json-false) ("started" :json-false)))
    (should (equal "Importing" (jb-kotlin--import-label (jb-kotlin--import-state workspace))))))

(ert-deftest jb-kotlin-import-reload-response-does-not-clear-failure ()
  (jb-kotlin-test--reload
    (jb-kotlin-test--save-build root "pom.xml")
    (jb-kotlin-test--flush workspace)
    (jb-kotlin-test--import-event workspace :failed "broken build")
    (funcall (plist-get (car requests) :success) nil)
    (should (equal "Import failed" (jb-kotlin--import-label (jb-kotlin--import-state workspace))))))

(ert-deftest jb-kotlin-import-success-of-another-folder-does-not-hide-failure ()
  (jb-kotlin-test--reload
    (jb-kotlin-test--import-event workspace :failed "module one failed")
    (jb-kotlin-test--import-event workspace :started "module two started")
    (jb-kotlin-test--import-event workspace :succeeded "module two succeeded")
    (should (equal "Import failed" (jb-kotlin--import-label (jb-kotlin--import-state workspace))))
    (jb-kotlin--import-state-notification
     workspace (jb-kotlin-test--object
                ("phase" "FINISHED")
                ("folders" (vector (jb-kotlin-test--object ("folderUri" "file:///module")
                                                         ("status" "SUCCESS"))))))
    (should (equal "Ready" (jb-kotlin--import-label (jb-kotlin--import-state workspace))))))

(ert-deftest jb-kotlin-import-folder-failure-is-visible-without-log-flags ()
  (jb-kotlin-test--reload
    (jb-kotlin--import-state-notification
     workspace (jb-kotlin-test--object
                ("phase" "FINISHED")
                ("folders" (vector (jb-kotlin-test--object ("folderUri" "file:///module")
                                                         ("status" "FAILED") ("message" "Missing SDK"))))))
    (should (equal "Import failed" (jb-kotlin--import-label (jb-kotlin--import-state workspace))))
    (save-window-excursion
      (jb-kotlin-import-status workspace)
      (should buffer-read-only)
      (should (string-match-p "Missing SDK" (buffer-string)))
      (should (next-button (point-min))))))

(ert-deftest jb-kotlin-import-blocked-state-is-visible-without-forcing-a-prompt ()
  (jb-kotlin-test--reload
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (ert-fail "Unexpected prompt"))))
      (dolist (dismissed '(:json-false t))
        (jb-kotlin--import-status-notification workspace (jb-kotlin-test--blocked dismissed))
        (should (equal "Build tool selection required"
                       (jb-kotlin--import-label (jb-kotlin--import-state workspace)))))
      (save-window-excursion
        (jb-kotlin-import-status workspace)
        (should (string-match-p "selection dismissed" (buffer-string)))
        (should (string-match-p "gradle, maven" (buffer-string)))))
    (jb-kotlin--import-status-notification workspace (jb-kotlin-test--object ("blockedFolders" [])))
    (should-not (jb-kotlin--import-state-blocked (jb-kotlin--import-state workspace)))))

(ert-deftest jb-kotlin-import-seed-does-not-overwrite-live-status-or-revive-disconnect ()
  (jb-kotlin-test--reload
    (let (callback)
      (cl-letf (((symbol-function 'lsp-request-async)
                 (lambda (method _params success &rest _)
                   (should (equal "intellij/workspaceImportStatus" method))
                   (setq callback success))))
        (jb-kotlin--import-initialized workspace)
        (jb-kotlin--import-status-notification workspace (jb-kotlin-test--blocked t))
        (funcall callback (jb-kotlin-test--object ("blockedFolders" [])))
        (should (jb-kotlin--import-state-blocked (jb-kotlin--import-state workspace)))
        (jb-kotlin--import-disconnected workspace)
        (funcall callback (jb-kotlin-test--blocked t))
        (should-not (gethash workspace jb-kotlin--import-states))))))

(ert-deftest jb-kotlin-import-unsupported-status-request-is-optional ()
  (jb-kotlin-test--reload
    (cl-letf (((symbol-function 'lsp-request-async)
               (lambda (_method _params _success &rest keys)
                 (funcall (plist-get keys :error-handler) '(:code -32601)))))
      (jb-kotlin--import-initialized workspace))
    (jb-kotlin-test--import-event workspace :succeeded "import finished")
    (should (equal "Ready" (jb-kotlin--import-label (jb-kotlin--import-state workspace))))))

(ert-deftest jb-kotlin-import-retry-from-log-preserves-workspace-and-settings ()
  (jb-kotlin-test--reload
    (let ((jb-kotlin-projects [(:type "gradle" :path "file:///correct")]))
      (jb-kotlin--import-reloading workspace (jb-kotlin--initialization-options)))
    (jb-kotlin-test--import-event workspace :failed "failed")
    (save-window-excursion
      (jb-kotlin-show-import-log workspace)
      (let ((jb-kotlin-projects []) (lsp--buffer-workspaces (list other)))
        (jb-kotlin-retry-import))
      (should (eq workspace (plist-get (car requests) :workspace)))
      (should (equal [(:type "gradle" :path "file:///correct")]
                     (plist-get (plist-get (plist-get (car requests) :params) :initializationOptions) :projects)))
      (should (equal "Importing" (jb-kotlin--import-label (jb-kotlin--import-state workspace))))
      (jb-kotlin-test--import-event workspace :succeeded "fixed")
      (should (equal "Ready" (jb-kotlin--import-label (jb-kotlin--import-state workspace)))))))

(ert-deftest jb-kotlin-import-stopped-log-remains-readable-and-retry-refuses ()
  (jb-kotlin-test--reload
    (jb-kotlin-test--import-event workspace :failed "retained error")
    (let ((state (jb-kotlin--import-state workspace)))
      (save-window-excursion
        (jb-kotlin-import-status workspace)
        (setf (lsp--workspace-status workspace) 'shutdown)
        (jb-kotlin--import-disconnected workspace)
        (should (string-match-p "Stopped" (buffer-string)))
        (should-error (jb-kotlin-retry-import) :type 'user-error)
        (jb-kotlin-show-import-log)
        (should (string-match-p "retained error" (buffer-string)))
        (should-not requests))
      (kill-buffer (jb-kotlin--import-state-view state))
      (kill-buffer (jb-kotlin--import-state-log state)))))

(provide 'jb-kotlin-import-test)
;;; jb-kotlin-import-test.el ends here

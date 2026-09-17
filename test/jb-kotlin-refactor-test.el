;;; jb-kotlin-refactor-test.el --- Refactoring UI tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'jb-kotlin-lsp-dap)

(defmacro jb-kotlin-test--refactor (&rest body)
  "Run BODY with a private file, workspace, UI queues and controlled timers."
  (declare (indent 0))
  `(jb-kotlin-test--workspace
    (let* ((file (make-temp-file "jb-refactor-" nil ".kt" "fun value() = 1\n"))
           (source (find-file-noselect file))
           (jb-kotlin--conflicts nil) (jb-kotlin--choice-queue nil)
           (jb-kotlin--choice-busy nil) timers replies)
      (setf (lsp--workspace-status workspace) 'initialized)
      (unwind-protect
          (save-window-excursion
            (switch-to-buffer source)
            (setq-local lsp--buffer-workspaces (list workspace))
            (cl-letf (((symbol-function 'run-with-idle-timer)
                       (lambda (_time _repeat callback &rest args)
                         (let ((timer (cons callback args))) (push timer timers) timer)))
                      ((symbol-function 'run-at-time)
                       (lambda (_time _repeat callback &rest args)
                         (let ((timer (cons callback args))) (push timer timers) timer))))
              ,@body))
        (dolist (conflict (copy-sequence jb-kotlin--conflicts))
          (jb-kotlin--conflict-finish conflict "cancel"))
        (when (buffer-live-p source)
          (with-current-buffer source (set-buffer-modified-p nil)) (kill-buffer source))
        (delete-file file)))))

(defun jb-kotlin-test--conflict-params (&optional location)
  "Build a wire-format conflict containing an optional LOCATION."
  (jb-kotlin-test--object
   ("title" "Conflicting refactoring") ("continueLabel" "Apply anyway") ("cancelLabel" "Stop")
   ("conflicts" (vector (jb-kotlin-test--object ("messages" ["A conflict" "Another detail"])
						("location" location))))))

(ert-deftest jb-kotlin-refactor-conflicts-are-async-and-answer-once ()
  (jb-kotlin-test--refactor
   (cl-letf (((symbol-function 'lsp--send-request-response)
              (lambda (w _time request response)
                (should (eq w workspace))
                (push (cons (lsp-get request :id) response) replies)
                ;; A server edit arriving after acceptance must not cancel twice.
                (with-current-buffer source (insert "// edit\n")))))
     (lsp--on-request workspace
                      (jb-kotlin-test--object ("jsonrpc" "2.0") ("id" 42)
                                              ("method" "intellij/showConflicts")
                                              ("params" (jb-kotlin-test--conflict-params))))
     (should-not replies)
     (let ((conflict (car jb-kotlin--conflicts)))
       (with-current-buffer (jb-kotlin--conflict-buffer conflict)
         (should buffer-read-only)
         (should (string-match-p "Another detail" (buffer-string)))
         (jb-kotlin-continue-refactoring))
       (jb-kotlin--conflict-finish conflict "cancel"))
     (should (equal replies '((42 :decision "continue")))))
   (should-not jb-kotlin--conflicts)))

(ert-deftest jb-kotlin-refactor-killing-review-cancels ()
  (jb-kotlin-test--refactor
   (jb-kotlin--show-conflicts workspace (jb-kotlin-test--conflict-params) (lambda (r) (push r replies)))
   (kill-buffer (jb-kotlin--conflict-buffer (car jb-kotlin--conflicts)))
   (should (equal replies '((:decision "cancel"))))))

(ert-deftest jb-kotlin-refactor-hiding-review-cancels ()
  (jb-kotlin-test--refactor
   (jb-kotlin--show-conflicts workspace (jb-kotlin-test--conflict-params) (lambda (r) (push r replies)))
   (dolist (window (get-buffer-window-list (jb-kotlin--conflict-buffer (car jb-kotlin--conflicts)) nil t))
     (set-window-buffer window source))
   (jb-kotlin--conflict-windows-changed)
   (should (equal replies '((:decision "cancel"))))))

(ert-deftest jb-kotlin-refactor-file-edits-cancel-but-log-output-does-not ()
  (jb-kotlin-test--refactor
   (jb-kotlin--show-conflicts workspace (jb-kotlin-test--conflict-params) (lambda (r) (push r replies)))
   (with-temp-buffer (insert "server log output"))
   (should-not replies)
   (with-current-buffer source (insert "// user edit\n"))
   (should (equal replies '((:decision "cancel"))))))

(ert-deftest jb-kotlin-refactor-continue-rechecks-edits-with-inhibited-hooks ()
  (jb-kotlin-test--refactor
   (jb-kotlin--show-conflicts workspace (jb-kotlin-test--conflict-params) (lambda (r) (push r replies)))
   (let ((conflict (car jb-kotlin--conflicts)))
     (with-current-buffer source
       (let ((inhibit-modification-hooks t)) (insert "// hidden edit\n")))
     (should-not replies)
     (jb-kotlin--conflict-finish conflict "continue"))
   (should (equal replies '((:decision "cancel"))))))

(ert-deftest jb-kotlin-refactor-disk-changes-cancel ()
  (jb-kotlin-test--refactor
   (jb-kotlin--show-conflicts workspace (jb-kotlin-test--conflict-params) (lambda (r) (push r replies)))
   (write-region "changed on disk\n" nil file nil 'silent)
   (jb-kotlin--conflict-finish (car jb-kotlin--conflicts) "continue")
   (should (equal replies '((:decision "cancel"))))))

(ert-deftest jb-kotlin-refactor-server-cancellation-is-scoped-by-workspace-and-id ()
  (jb-kotlin-test--refactor
   (let ((jb-kotlin--conflict-request-id "request-1"))
     (jb-kotlin--show-conflicts workspace (jb-kotlin-test--conflict-params) (lambda (r) (push r replies))))
   (jb-kotlin--conflict-cancel-request workspace (jb-kotlin-test--object ("id" "other")))
   (jb-kotlin--conflict-cancel-request (make-lsp--workspace) (jb-kotlin-test--object ("id" "request-1")))
   (should-not replies)
   (jb-kotlin--conflict-cancel-request workspace (jb-kotlin-test--object ("id" "request-1")))
   (should (equal replies '((:decision "cancel"))))))

(ert-deftest jb-kotlin-refactor-disconnect-cancels-conflicts-and-queued-menus ()
  (jb-kotlin-test--refactor
   (jb-kotlin--show-conflicts workspace (jb-kotlin-test--conflict-params) (lambda (r) (push r replies)))
   (jb-kotlin--choose-action workspace nil)
   (run-hook-with-args 'lsp-after-uninitialized-functions workspace)
   (should-not jb-kotlin--choice-queue)
   (should (equal replies '((:decision "cancel"))))))

(ert-deftest jb-kotlin-refactor-clipboard-preserves-text-and-does-not-run-code ()
  (let ((kill-ring nil) (interprogram-cut-function nil))
    (jb-kotlin--copy-to-clipboard nil (jb-kotlin-test--object ("content" "(delete-file \"never\")\nλ")))
    (should (equal (car kill-ring) "(delete-file \"never\")\nλ"))))

(ert-deftest jb-kotlin-refactor-editor-command-checks-uri-caret-and-allowlist ()
  (jb-kotlin-test--refactor
   (let ((calls 0))
     (cl-letf (((symbol-function 'lsp-rename) (lambda () (interactive) (cl-incf calls))))
       (dolist (uri (list (lsp--buffer-uri) "file:///wrong.kt"))
         (jb-kotlin--run-editor-command workspace (jb-kotlin-test--object ("command" "editor.action.rename") ("uri" uri)))
         (apply (caar timers) (cdar timers)))
       (should (= calls 1))
       (jb-kotlin--run-editor-command workspace (jb-kotlin-test--object ("command" "editor.action.rename")))
       (forward-char 1)
       (apply (caar timers) (cdar timers))
       (should (= calls 1))
       (let ((count (length timers)))
         (jb-kotlin--run-editor-command workspace (jb-kotlin-test--object ("command" "delete-file")))
         (should (= count (length timers))))))))

(defun jb-kotlin-test--choices ()
  "Build two same-named menu choices with distinct server commands."
  (jb-kotlin-test--object
   ("title" "Choose")
   ("entries" (vector
               (jb-kotlin-test--object ("name" "Variant") ("command" (jb-kotlin-test--object ("command" "first") ("arguments" [1]))))
               (jb-kotlin-test--object ("name" "Variant") ("command" (jb-kotlin-test--object ("command" "second") ("arguments" [2]))))))))

(ert-deftest jb-kotlin-refactor-choice-routes-commands-and-allows-nested-menus ()
  (jb-kotlin-test--refactor
   (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "2. Variant"))
             ((symbol-function 'lsp-request-async)
              (lambda (method params _callback &rest _)
                (should (eq lsp--cur-workspace workspace))
                (should (equal method "workspace/executeCommand"))
                (push params replies)
                (when (= (length replies) 1) (jb-kotlin--choose-action workspace (jb-kotlin-test--choices))))))
     (jb-kotlin--choose-action workspace (jb-kotlin-test--choices))
     (jb-kotlin--show-next-action)
     (jb-kotlin--show-next-action)
     (should (equal replies '((:command "second" :arguments [2]) (:command "second" :arguments [2])))))))

(ert-deftest jb-kotlin-refactor-choice-cancel-or-edit-sends-nothing ()
  (jb-kotlin-test--refactor
   (cl-letf (((symbol-function 'lsp-request-async) (lambda (&rest _) (ert-fail "Unexpected action"))))
     (dolist (behavior '(quit edit))
       (jb-kotlin--choose-action workspace (jb-kotlin-test--choices))
       (cl-letf (((symbol-function 'completing-read)
                  (lambda (&rest _)
                    (if (eq behavior 'quit) (signal 'quit nil)
                      (with-current-buffer source (insert "// edit\n")) "1. Variant"))))
         (jb-kotlin--show-next-action)))
     (should-not jb-kotlin--choice-busy))))

(ert-deftest jb-kotlin-refactor-undisplayable-review-cancels ()
  (jb-kotlin-test--refactor
   (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil)))
     (jb-kotlin--show-conflicts workspace (jb-kotlin-test--conflict-params) (lambda (r) (push r replies))))
   (should (equal replies '((:decision "cancel"))))))

(ert-deftest jb-kotlin-refactor-reveal-keeps-review-and-does-not-cancel ()
  (jb-kotlin-test--refactor
   (let* ((target (make-temp-file "jb-conflict-target-" nil ".kt" "fun target() = 2\n"))
          (location (jb-kotlin-test--object
                     ("uri" (lsp--path-to-uri target))
                     ("range" (jb-kotlin-test--object
                               ("start" (jb-kotlin-test--object ("line" 0) ("character" 4))))))))
     (unwind-protect
         (progn
           (jb-kotlin--show-conflicts workspace (jb-kotlin-test--conflict-params location) (lambda (r) (push r replies)))
           (let* ((conflict (car jb-kotlin--conflicts))
                  (review (jb-kotlin--conflict-buffer conflict)))
             (select-window (get-buffer-window review))
             (with-current-buffer review
               (button-activate (next-button (point-min))))
             (jb-kotlin--conflict-windows-changed)
             (should (get-buffer-window review))
             (should-not replies)
             (should (equal (buffer-file-name (window-buffer (selected-window))) target))
             (jb-kotlin--conflict-finish conflict "continue")
             (should (equal replies '((:decision "continue"))))))
       (when-let* ((buffer (get-file-buffer target))) (kill-buffer buffer))
       (delete-file target)))))

(provide 'jb-kotlin-refactor-test)
;;; jb-kotlin-refactor-test.el ends here

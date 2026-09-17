;;; navigation-smoke.el --- Isolated live Kotlin LSP test -*- lexical-binding: t; -*-

;; Run in a NEW batch Emacs, never with emacsclient. See test/README.org.
(require 'jb-kotlin-lsp-dap)
(require 'lsp-lens)
(require 'lsp-modeline)
(require 'lsp-headerline)
(require 'lsp-diagnostics)
(require 'lsp-completion)
(require 'ert)

(defvar jb-kotlin-smoke-root
  (file-name-as-directory (or (getenv "JB_KOTLIN_TEST_ROOT")
                             (error "Set JB_KOTLIN_TEST_ROOT to generated fixtures"))))
(defvar jb-kotlin-smoke-kind (or (getenv "JB_KOTLIN_TEST_PROJECT") "binary"))
(defvar jb-kotlin-smoke-state
  (file-name-as-directory
   (make-temp-file (expand-file-name (concat "emacs-" jb-kotlin-smoke-kind "-")
                                    jb-kotlin-smoke-root) t)))
(make-directory jb-kotlin-smoke-state t)
(setq user-emacs-directory jb-kotlin-smoke-state
      lsp-session-file (expand-file-name "session" jb-kotlin-smoke-state)
      lsp-auto-guess-root nil
      lsp-enable-file-watchers nil
      lsp-enable-snippet nil
      lsp-enable-dap-auto-configure nil
      lsp-enable-indentation nil
      lsp-enable-on-type-formatting nil
      lsp-enabled-clients '(jb-kotlin)
      lsp-response-timeout 90
      lsp-log-io t
      lsp-restart 'ignore
      jb-kotlin-default-sdk (string-trim (with-temp-buffer
                                          (insert-file-contents (expand-file-name "jdk-home" jb-kotlin-smoke-root))
                                          (buffer-string)))
      jb-kotlin-server-command
      (list "kotlin-lsp" "--stdio"
            (concat "--system-path=" (expand-file-name "system" jb-kotlin-smoke-state))))
(setenv "IJ_JAVA_OPTIONS"
        (concat (getenv "IJ_JAVA_OPTIONS")
                " -Didea.config.path=" (expand-file-name "config" jb-kotlin-smoke-state)
                " -Didea.log.path=" (expand-file-name "log" jb-kotlin-smoke-state)
                " -Djava.util.prefs.userRoot=" (expand-file-name "prefs" jb-kotlin-smoke-state)))

;; The test needs a Kotlin language ID, not a third-party major-mode install.
(define-derived-mode jb-kotlin-smoke-mode prog-mode "Kotlin-smoke")
(add-to-list 'lsp-language-id-configuration '(jb-kotlin-smoke-mode . "kotlin"))
(push 'jb-kotlin-smoke-mode (lsp--client-major-modes (gethash 'jb-kotlin lsp-clients)))

(defun jb-kotlin-smoke-definition (name)
  "Find definition of NAME in the fixture, waiting for the import to finish."
  (goto-char (point-min))
  (search-forward name)
  (backward-char (length name))
  (let ((deadline (+ (float-time) 90))
        (params (lsp--text-document-position-params)) result)
    (while (and (not result) (< (float-time) deadline))
      (setq result (lsp-request "textDocument/definition" params))
      (unless (and result (> (length result) 0))
        (setq result nil)
        (accept-process-output nil 0.5)))
    (unless result (error "No definition for %s; see %s" name jb-kotlin-smoke-state))
    (elt result 0)))

(ert-deftest jb-kotlin-live-navigation ()
 (let* ((project (expand-file-name jb-kotlin-smoke-kind jb-kotlin-smoke-root))
       (default-directory (file-name-as-directory project))
       (source (find-file-noselect (expand-file-name "src/Main.kt" project)))
       workspace)
  (unwind-protect
      (with-current-buffer source
        (jb-kotlin-smoke-mode)
        (setq jb-kotlin-projects (vector (list :type "json" :path (lsp--path-to-uri project))))
        (lsp-workspace-folders-add project)
        (lsp)
        (let ((deadline (+ (float-time) 90)))
          (while (and (< (float-time) deadline)
                      (not (cl-some (lambda (w) (eq 'initialized (lsp--workspace-status w)))
                                    (lsp-workspaces))))
            (accept-process-output nil 0.2)))
        (setq workspace (jb-kotlin--workspace))
        (should (eq 'initialized (lsp--workspace-status workspace)))
        (princ (format "Emacs PID=%d; LSP PID=%d; project=%s\n"
                       (emacs-pid) (process-id (lsp--workspace-proc workspace)) project))
        (princ (format "JDK target ready: %S\n" (jb-kotlin-smoke-definition "ArrayList")))
        (let* ((location (jb-kotlin-smoke-definition "Greeter"))
               (uri (lsp--location-uri location))
               (range (lsp--location-range location))
               (before-open
                (let ((code (lsp-get (jb-kotlin--command "decompile" uri) :code)))
                  (with-temp-buffer
                    (insert code)
                    (goto-char (point-min))
                    (search-forward "String")
                    (backward-char 3)
                    (with-lsp-workspace workspace
                      (lsp-request "textDocument/definition"
                                   (list :textDocument (list :uri uri)
                                         :position (lsp--cur-position)))))))
               (path (lsp--uri-to-path uri)))
          (princ (format "Library definition before didOpen: %S\n" before-open))
          (princ (format "Library definition: %s\n" uri))
          (should (string-prefix-p "jar:" uri))
          (should (string-match-p "Greeter\\.\\(?:class\\|java\\)" uri))
          ;; Exercise the same xref conversion used by M-., not just our handler.
          (should (lsp--locations-to-xref-items (vector location)))
          (with-current-buffer (get-file-buffer path)
            (should buffer-read-only)
            (should (equal uri (lsp--buffer-uri)))
            (should (eq workspace (jb-kotlin--workspace)))
            (should (string-match-p "class Greeter" (buffer-string)))
            (write-region (point-min) (point-max)
                          (expand-file-name "decompiled.java" jb-kotlin-smoke-state) nil 'silent)
            (goto-char (point-min))
            (search-forward "toString(")
            (backward-char 3)
            (let ((hover (lsp-request "textDocument/hover" (lsp--text-document-position-params))))
              (princ (format "Hover request: %S\n" (lsp--text-document-position-params)))
              (princ (format "Library hover: %S\n" hover))
              (when (string-suffix-p ".java" uri)
                (let ((markdown (lsp-get (lsp-get hover :contents) :value)))
                  (should (string-match "command:[^)]*" markdown))
                  (let ((link (match-string 0 markdown)))
                    (save-current-buffer
                      (save-window-excursion
                        (lsp--document-link-handle-target link)
                        (should (string-suffix-p "/Object.java" (lsp--buffer-uri)))
                        (should (looking-at "toString"))))))))
            (goto-char (point-min))
            (search-forward "String")
            (backward-char 3)
            (let ((definitions (lsp-request "textDocument/definition" (lsp--text-document-position-params))))
              (princ (format "Library follow-up definition: %S\n" definitions))
              (should (equal definitions before-open))))
          (should (equal path (lsp--uri-to-path uri)))
          (kill-buffer (get-file-buffer path))
          (should (equal path (lsp--uri-to-path uri)))
          (save-window-excursion
            (let ((start (lsp-get range :start)))
              (lsp--document-link-handle-target
               (concat "command:jetbrains.navigateToLocation?"
                       (url-hexify-string
                        (json-encode (vector uri (lsp-get start :line) (lsp-get start :character)))))))
            (should (equal uri (lsp--buffer-uri)))))
        (when (equal jb-kotlin-smoke-kind "sources")
          (let* ((uri (concat "jar://" (expand-file-name "greeter-sources.jar" jb-kotlin-smoke-root)
                              "!/demo/Greeter.java"))
                 (path (lsp--uri-to-path uri)))
            (with-current-buffer (get-file-buffer path)
              (should buffer-read-only)
              (should (string-match-p "A tiny dependency" (buffer-string))))))
        (let* ((location (jb-kotlin-smoke-definition "ArrayList"))
               (uri (lsp--location-uri location))
               (path (lsp--uri-to-path uri)))
          (princ (format "JDK definition: %s\n" uri))
          (should (string-match-p "\\`\\(?:jar\\|jrt\\):" uri))
          (with-current-buffer (get-file-buffer path)
            (should buffer-read-only)
            (should (string-match-p "class ArrayList" (buffer-string)))))
        ;; Exercise jrt even when this JDK's attached src.zip wins definition lookup.
        (let* ((uri (concat "jrt://" jb-kotlin-default-sdk "!/java.base/java/lang/Object.class"))
               (path (lsp--uri-to-path uri)))
          (with-current-buffer (get-file-buffer path)
            (should (string-match-p "class Object" (buffer-string)))))
        (princ (format "PASS: %s definitions, decompilation, xref, cache/reopen, links, JRT\n"
                       jb-kotlin-smoke-kind)))
    (when workspace
      (when-let* ((log (lsp--workspace-ewoc workspace))
                  (io (ewoc-buffer log)))
        (with-current-buffer io
          (write-region (point-min) (point-max)
                        (expand-file-name "lsp-io.log" jb-kotlin-smoke-state) nil 'silent)))
      (lsp-workspace-shutdown workspace)
      (let ((deadline (+ (float-time) 10)))
        (while (and (process-live-p (lsp--workspace-proc workspace)) (< (float-time) deadline))
          (accept-process-output nil 0.1))
        (when (process-live-p (lsp--workspace-proc workspace))
          (delete-process (lsp--workspace-proc workspace))))))))

(ert-run-tests-batch-and-exit 'jb-kotlin-live-navigation)

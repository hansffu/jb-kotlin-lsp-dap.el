;;; jb-kotlin-navigation.el --- Kotlin dependency sources -*- lexical-binding: t; -*-

;;; Commentary:
;; URI and command handlers for dependency sources and documentation links.

;;; Code:

(require 'cl-lib)
(require 'lsp-mode)
(require 'url-util)
(require 'xref)

(declare-function jb-kotlin--command "jb-kotlin-lsp-dap")

(defvar jb-kotlin--source-caches (make-hash-table :test 'eq)
  "Workspace to URI/source metadata tables, private to this Emacs session.")
(defvar jb-kotlin--source-directories nil
  "Temporary source directories created by this Emacs session.")

(defun jb-kotlin--navigation-workspace ()
  "Find a Kotlin workspace, asking when a help buffer has ambiguous context."
  (let* ((kotlin-p (lambda (w)
                     (eq 'jb-kotlin
                         (lsp--client-server-id (lsp--workspace-client w)))))
         (local (cl-find-if kotlin-p (lsp-workspaces)))
         (candidates (or (and local (list local))
                         (cl-remove-if-not
                          (lambda (w)
                            (and (funcall kotlin-p w)
                                 (eq 'initialized (lsp--workspace-status w))))
                          (lsp--session-workspaces (lsp-session))))))
    (pcase candidates
      (`() (user-error "Start JetBrains Kotlin LSP before opening dependency sources"))
      (`(,workspace) workspace)
      (_ (let ((choices (mapcar (lambda (w) (cons (lsp--workspace-root w) w))
                               candidates)))
           (cdr (assoc (completing-read "Kotlin workspace: " choices nil t)
                       choices)))))))

(defun jb-kotlin--source-mode (language)
  "Choose an available major mode for decompiled LANGUAGE."
  (pcase language
    ("java" #'java-mode)
    ("kotlin" (cond ((fboundp 'kotlin-mode) #'kotlin-mode)
                    ((fboundp 'kotlin-ts-mode) #'kotlin-ts-mode)
                    (t #'prog-mode)))
    (_ #'prog-mode)))

(defun jb-kotlin--source-buffer (file uri language workspace)
  "Visit cached FILE as read-only URI in LANGUAGE, attached to WORKSPACE."
  (or (get-file-buffer file)
      (let ((buffer (generate-new-buffer (file-name-nondirectory file))))
        (condition-case err
            (with-current-buffer buffer
              (insert-file-contents file)
              ;; Do not run user major-mode hooks that could start another server.
              (delay-mode-hooks (funcall (jb-kotlin--source-mode language)))
              (set-visited-file-name file t)
              (setq-local lsp-buffer-uri uri)
              (setq-local lsp-language-id-configuration
                          (cons (cons major-mode language) lsp-language-id-configuration))
              (setq-local lsp--buffer-workspaces (list workspace))
              (when (fboundp 'lsp--set-position-encoding)
                (lsp--set-position-encoding
                 (or (lsp-get (lsp--workspace-server-capabilities workspace)
                              :positionEncoding)
                     "utf-16")))
              (setq-local buffer-read-only t)
              (setq-local buffer-auto-save-file-name nil)
              (set-buffer-modified-p nil)
              (lsp-mode 1)
              (lsp--open-in-workspace workspace)
              buffer)
          (error (kill-buffer buffer)
                 (signal (car err) (cdr err)))))))

(defun jb-kotlin--source-uri (uri)
  "Return a cached file for jar/jrt URI and prepare its source buffer."
  (unless (and (stringp uri) (string-match-p "\\`\\(?:jar\\|jrt\\):" uri))
    (user-error "Unsupported Kotlin source URI: %s" uri))
  (let* ((workspace (jb-kotlin--navigation-workspace))
         (cache (or (gethash workspace jb-kotlin--source-caches)
                    (puthash workspace (make-hash-table :test 'equal)
                             jb-kotlin--source-caches)))
         (entry (gethash uri cache)))
    (unless (and entry (file-readable-p (car entry)))
      (let* ((response (with-lsp-workspace workspace
                         (jb-kotlin--command "decompile" uri)))
             (code (and response (lsp-get response :code)))
             (language (and response (lsp-get response :language))))
        (unless (and (stringp code) (stringp language))
          (user-error "Kotlin server could not decompile %s" uri))
        (let* ((directory (or (gethash :directory cache)
                              (let ((dir (make-temp-file "jb-kotlin-sources-" t)))
                                (push dir jb-kotlin--source-directories)
                                (puthash :directory dir cache))))
               (name (file-name-base (car (split-string uri "[?#]"))))
               (file (expand-file-name
                      (concat (replace-regexp-in-string "[^[:alnum:]_.-]" "_" name)
                              "-" (substring (secure-hash 'sha256 uri) 0 16)
                              (pcase language ("java" ".java") ("kotlin" ".kt") (_ ".txt")))
                      directory)))
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region code nil file nil 'silent))
          (setq entry (cons file language))
          ;; didOpen can resolve the URI again; publish the cache first.
          (puthash uri entry cache))))
    (jb-kotlin--source-buffer (car entry) uri (cdr entry) workspace)
    (car entry)))

(defun jb-kotlin--navigate (uri line character)
  "Visit URI at zero-based LSP LINE and CHARACTER."
  (unless (and (stringp uri) (string-match-p "\\`\\(?:file\\|jar\\|jrt\\):" uri)
               (integerp line) (>= line 0) (integerp character) (>= character 0))
    (user-error "Invalid Kotlin navigation target"))
  (let* ((workspace (cl-find-if
                     (lambda (w) (eq 'jb-kotlin
                                      (lsp--client-server-id (lsp--workspace-client w))))
                     (lsp-workspaces)))
         (encoding (and workspace
                        (lsp-get (lsp--workspace-server-capabilities workspace)
                                 :positionEncoding)))
         (file (if (string-prefix-p "file:" uri)
                   (lsp--uri-to-path uri)
                 (jb-kotlin--source-uri uri))))
    (xref-push-marker-stack)
    (pop-to-buffer (find-file-noselect file))
    ;; A regular file opened from a help buffer may not have LSP configured yet.
    ;; Decode coordinates even on lsp-mode versions without encoding negotiation.
    (goto-char (point-min))
    (forward-line line)
    (while (and (> character 0) (not (eolp)))
      (setq character
            (- character
               (pcase encoding
                 ("utf-8" (string-bytes (char-to-string (char-after))))
                 ("utf-32" 1)
                 (_ (if (> (char-after) #xffff) 2 1)))))
      (forward-char 1))))

(defun jb-kotlin--navigate-action (command)
  "Handle a jetbrains.navigateToLocation COMMAND."
  (let ((args (lsp-get command :arguments)))
    (unless (= (length args) 3)
      (user-error "Kotlin navigation requires URI, line and character"))
    (apply #'jb-kotlin--navigate (append args nil))))

(defun jb-kotlin--command-uri (uri)
  "Handle a navigation command URI from hover documentation.
Only the known navigation command is accepted."
  (unless (string-match "\\`command:jetbrains\\.navigateToLocation[?]\\(.*\\)\\'" uri)
    (user-error "Unsupported Kotlin documentation command"))
  (let ((args (condition-case nil
                  (json-parse-string
                   (decode-coding-string (url-unhex-string (match-string 1 uri)) 'utf-8)
                   :array-type 'list)
                (error (user-error "Invalid Kotlin navigation link")))))
    (unless (and (listp args) (= (length args) 3))
      (user-error "Kotlin navigation requires URI, line and character"))
    (apply #'jb-kotlin--navigate args)))

(defun jb-kotlin--clear-source-caches ()
  "Remove temporary source files owned by this Emacs session."
  (dolist (directory jb-kotlin--source-directories)
    (when (file-directory-p directory)
      (delete-directory directory t)))
  (setq jb-kotlin--source-directories nil)
  (clrhash jb-kotlin--source-caches))

(add-hook 'kill-emacs-hook #'jb-kotlin--clear-source-caches)

(provide 'jb-kotlin-navigation)
;;; jb-kotlin-navigation.el ends here

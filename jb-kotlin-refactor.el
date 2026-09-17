;;; jb-kotlin-refactor.el --- IntelliJ refactoring UI -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom editor interactions for IntelliJ ModCommand refactorings.
;; Conflict requests are asynchronous and fail closed when documents change.

;;; Code:

(require 'cl-lib)
(require 'lsp-mode)
(require 'jb-kotlin-navigation)
(require 'button)
(require 'eldoc)

(cl-defstruct (jb-kotlin--conflict (:constructor jb-kotlin--make-conflict))
  workspace id callback buffer snapshot files visible done)
(defvar jb-kotlin--conflicts nil "Unanswered conflict requests.")
(defvar jb-kotlin--conflict-request-id nil "Incoming request ID during dispatch.")
(defvar-local jb-kotlin--conflict nil "Conflict request shown by this buffer.")
(defvar jb-kotlin--choice-queue nil "Pending action menus.")
(defvar jb-kotlin--choice-busy nil "Non-nil while an action menu is open.")

(defun jb-kotlin--refactor-live-p (workspace)
  "Whether WORKSPACE can still accept refactoring decisions."
  (eq 'initialized (lsp--workspace-status workspace)))

(defun jb-kotlin--refactor-snapshot ()
  "Capture the modification ticks of open file buffers."
  (cl-loop for buffer in (buffer-list)
           when (buffer-local-value 'buffer-file-name buffer)
           collect (with-current-buffer buffer
                     (list buffer (buffer-chars-modified-tick) buffer-file-name))))

(defun jb-kotlin--refactor-unchanged-p (snapshot)
  "Whether file buffers in SNAPSHOT still have the same contents and identity."
  (cl-every (lambda (entry)
              (and (buffer-live-p (car entry))
                   (with-current-buffer (car entry)
                     (and (= (cadr entry) (buffer-chars-modified-tick))
                          (equal buffer-file-name (nth 2 entry))
                          (verify-visited-file-modtime (current-buffer))))))
            snapshot))

(defun jb-kotlin--copy-to-clipboard (_workspace params)
  "Copy PARAMS content to the kill ring and available system clipboard."
  (when-let* ((content (lsp-get params :content)) (_ (stringp content)))
    (let ((select-enable-clipboard t)) (kill-new content))))

(defconst jb-kotlin--editor-commands
  '(("editor.action.rename" . lsp-rename)
    ("editor.action.triggerSuggest" . completion-at-point)
    ("editor.action.triggerParameterHints" . eldoc-print-current-symbol-info))
  "Supported editor commands. Server strings are never evaluated as Lisp.")

(defun jb-kotlin--run-editor-command (workspace params)
  "Schedule a supported editor command from PARAMS in WORKSPACE."
  (let* ((command (cdr (assoc (lsp-get params :command) jb-kotlin--editor-commands)))
         (buffer (window-buffer (selected-window)))
         (position (with-current-buffer buffer (point)))
         (tick (with-current-buffer buffer (buffer-chars-modified-tick)))
         (uri (lsp-get params :uri)))
    (if (or (not command) (> (length (lsp-get params :arguments)) 0))
        (message "Kotlin: unsupported editor command %s" (lsp-get params :command))
      (run-at-time
       0 nil
       (lambda ()
         (when (and (jb-kotlin--refactor-live-p workspace) (buffer-live-p buffer)
                    (eq buffer (window-buffer (selected-window))))
           (with-current-buffer buffer
             (when (and (memq workspace (lsp-workspaces))
                        (= position (point)) (= tick (buffer-chars-modified-tick))
                        (or (null uri) (equal uri (lsp--buffer-uri))))
               (condition-case err
                   (call-interactively command)
                 (quit nil)
                 (error (message "Kotlin editor command: %s" (error-message-string err))))))))))))

(defun jb-kotlin--choose-action (workspace params)
  "Queue PARAMS' action menu for WORKSPACE outside the process filter."
  (setq jb-kotlin--choice-queue
        (nconc jb-kotlin--choice-queue
               (list (list workspace params (jb-kotlin--refactor-snapshot)))))
  (run-at-time 0 nil #'jb-kotlin--show-next-action))

(defun jb-kotlin--show-next-action ()
  "Show one queued menu, allowing follow-up menus from nested choices."
  (when (and jb-kotlin--choice-queue (not jb-kotlin--choice-busy))
    (let* ((jb-kotlin--choice-busy t)
           (item (pop jb-kotlin--choice-queue))
           (workspace (nth 0 item)) (params (nth 1 item)) (snapshot (nth 2 item)))
      (unwind-protect
          (when (and (jb-kotlin--refactor-live-p workspace)
                     (jb-kotlin--refactor-unchanged-p snapshot))
            (condition-case err
                (let* ((choices (cl-loop for entry across (vconcat (lsp-get params :entries))
                                         for n from 1
                                         collect (cons (format "%d. %s" n (lsp-get entry :name))
                                                       (lsp-get entry :command))))
                       (choice (and choices (completing-read
                                             (concat (or (lsp-get params :title) "Kotlin action") ": ")
                                             choices nil t)))
                       (command (cdr (assoc choice choices))))
                  (when (and command (jb-kotlin--refactor-live-p workspace)
                             (jb-kotlin--refactor-unchanged-p snapshot))
                    (with-lsp-workspace workspace
					(lsp-request-async "workspace/executeCommand"
							   (list :command (lsp-get command :command)
								 :arguments (or (lsp-get command :arguments) []))
							   #'ignore :mode 'detached))))
              (quit nil)
              (error (message "Kotlin action: %s" (error-message-string err)))))
        (when jb-kotlin--choice-queue (run-at-time 0 nil #'jb-kotlin--show-next-action))))))

(defvar jb-kotlin-conflicts-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "q") #'jb-kotlin-cancel-refactoring)
    (define-key map (kbd "C-g") #'jb-kotlin-cancel-refactoring)
    (define-key map (kbd "c") #'jb-kotlin-continue-refactoring)
    map))

(define-derived-mode jb-kotlin-conflicts-mode special-mode "Kotlin Conflicts"
  "Review conflicts; c continues, q or C-g cancels.")

(defun jb-kotlin--refactor-file-stamp (file)
  "Return FILE's content-change metadata, excluding access time."
  (when-let* ((attributes (file-attributes file)))
    (list (file-attribute-modification-time attributes)
          (file-attribute-status-change-time attributes)
          (file-attribute-size attributes)
          (file-attribute-inode-number attributes))))

(defun jb-kotlin--conflict-finish (conflict decision)
  "Answer CONFLICT once with DECISION, rejecting stale continuation."
  (unless (jb-kotlin--conflict-done conflict)
    (when (and (equal decision "continue")
               (not (and (jb-kotlin--refactor-live-p (jb-kotlin--conflict-workspace conflict))
                         (jb-kotlin--refactor-unchanged-p (jb-kotlin--conflict-snapshot conflict))
                         (cl-every (lambda (entry) (equal (cdr entry) (jb-kotlin--refactor-file-stamp (car entry))))
                                   (jb-kotlin--conflict-files conflict)))))
      (setq decision "cancel")
      (message "Kotlin: documents changed; refactoring cancelled"))
    ;; Detach before responding: the server may immediately send edits or a
    ;; second conflict request while the response is being processed.
    (setf (jb-kotlin--conflict-done conflict) t)
    (setq jb-kotlin--conflicts (delq conflict jb-kotlin--conflicts))
    (condition-case err
        (funcall (jb-kotlin--conflict-callback conflict) (list :decision decision))
      (error (message "Kotlin refactoring response: %s" (error-message-string err))))
    (when (buffer-live-p (jb-kotlin--conflict-buffer conflict))
      (kill-buffer (jb-kotlin--conflict-buffer conflict)))))

(defun jb-kotlin-continue-refactoring ()
  "Continue the refactoring shown in this buffer if its documents are unchanged."
  (interactive)
  (when jb-kotlin--conflict (jb-kotlin--conflict-finish jb-kotlin--conflict "continue")))

(defun jb-kotlin-cancel-refactoring ()
  "Cancel the refactoring shown in this buffer."
  (interactive)
  (when jb-kotlin--conflict (jb-kotlin--conflict-finish jb-kotlin--conflict "cancel")))

(defun jb-kotlin--conflict-buffer-killed ()
  "Cancel an unanswered request when its review buffer is killed."
  (when (and jb-kotlin--conflict (not (jb-kotlin--conflict-done jb-kotlin--conflict)))
    ;; Avoid recursively killing a buffer already running kill-buffer-hook.
    (setf (jb-kotlin--conflict-buffer jb-kotlin--conflict) nil)
    (jb-kotlin--conflict-finish jb-kotlin--conflict "cancel")))

(defun jb-kotlin--show-conflicts (workspace params callback)
  "Show PARAMS conflicts for WORKSPACE, answering through CALLBACK."
  (let* ((buffer (generate-new-buffer "*Kotlin refactoring conflicts*"))
         (conflict (jb-kotlin--make-conflict
                    :workspace workspace :id jb-kotlin--conflict-request-id
                    :callback callback :buffer buffer :snapshot (jb-kotlin--refactor-snapshot))))
    (push conflict jb-kotlin--conflicts)
    (condition-case err
        (with-current-buffer buffer
          (jb-kotlin-conflicts-mode)
          (setq jb-kotlin--conflict conflict)
          (add-hook 'kill-buffer-hook #'jb-kotlin--conflict-buffer-killed nil t)
          (let ((inhibit-read-only t))
            (insert (or (lsp-get params :title) "Refactoring conflicts") "\n\n")
            (mapc
             (lambda (entry)
               (when-let* ((location (lsp-get entry :location)))
                 (let* ((uri (lsp-get location :uri))
                        (start (lsp-get (lsp-get location :range) :start)))
                   (when (string-prefix-p "file:" uri)
                     (let ((file (lsp--uri-to-path uri)))
                       (unless (file-remote-p file)
                         (push (cons file (jb-kotlin--refactor-file-stamp file)) (jb-kotlin--conflict-files conflict)))))
                   (insert-text-button
                    (format "%s:%d" uri (1+ (lsp-get start :line)))
                    'follow-link t
                    'action (lambda (_)
                              (condition-case error
                                  (let ((display-buffer-overriding-action
                                         '(display-buffer-pop-up-window (inhibit-same-window . t))))
                                    (with-lsp-workspace workspace
							(jb-kotlin--navigate uri (lsp-get start :line) (lsp-get start :character))))
                                (error (message "Cannot open conflict: %s" (error-message-string error))))))
                   (insert "\n")))
               (mapc (lambda (text) (insert text "\n")) (lsp-get entry :messages))
               (insert "\n"))
             (lsp-get params :conflicts))
            (insert-text-button (or (lsp-get params :continueLabel) "Continue") 'follow-link t
                                'action (lambda (_) (jb-kotlin--conflict-finish conflict "continue")))
            (insert "    ")
            (insert-text-button (or (lsp-get params :cancelLabel) "Cancel") 'follow-link t
                                'action (lambda (_) (jb-kotlin--conflict-finish conflict "cancel")))
            (insert "\n\nAny file edit cancels this pending refactoring.\n")
            (goto-char (point-min)))
          (setf (jb-kotlin--conflict-visible conflict) (window-live-p (display-buffer buffer)))
          (unless (jb-kotlin--conflict-visible conflict)
            (jb-kotlin--conflict-finish conflict "cancel")))
      (error
       (jb-kotlin--conflict-finish conflict "cancel")
       (message "Kotlin conflicts: %s" (error-message-string err))))))

(defun jb-kotlin--conflict-dispatch (original workspace request)
  "Capture incoming conflict REQUEST IDs around ORIGINAL's WORKSPACE dispatch.
lsp-mode's asynchronous handler API omits the ID, needed for cancellation."
  (let ((jb-kotlin--conflict-request-id
         (and (equal "intellij/showConflicts" (lsp-get request :method)) (lsp-get request :id))))
    (funcall original workspace request)))

(defun jb-kotlin--conflict-cancel-request (workspace params)
  "Honor server cancellation PARAMS for WORKSPACE's pending request."
  (dolist (conflict (copy-sequence jb-kotlin--conflicts))
    (when (and (eq workspace (jb-kotlin--conflict-workspace conflict))
               (equal (lsp-get params :id) (jb-kotlin--conflict-id conflict)))
      (jb-kotlin--conflict-finish conflict "cancel"))))

(defun jb-kotlin--conflict-file-changed (&rest _)
  "Cancel pending conflicts after any file buffer is edited."
  (when buffer-file-name
    (dolist (conflict (copy-sequence jb-kotlin--conflicts))
      (jb-kotlin--conflict-finish conflict "cancel"))))

(defun jb-kotlin--conflict-windows-changed ()
  "Treat dismissing every window showing a conflict review as cancellation."
  (dolist (conflict (copy-sequence jb-kotlin--conflicts))
    (when (and (jb-kotlin--conflict-visible conflict)
               (not (get-buffer-window (jb-kotlin--conflict-buffer conflict) t)))
      (jb-kotlin--conflict-finish conflict "cancel"))))

(defun jb-kotlin--refactor-disconnected (workspace)
  "Cancel pending decisions when WORKSPACE stops."
  (dolist (conflict (copy-sequence jb-kotlin--conflicts))
    (when (eq workspace (jb-kotlin--conflict-workspace conflict))
      (jb-kotlin--conflict-finish conflict "cancel")))
  (setq jb-kotlin--choice-queue (cl-remove workspace jb-kotlin--choice-queue :key #'car :test #'eq)))

(advice-add 'lsp--on-request :around #'jb-kotlin--conflict-dispatch)
(add-hook 'after-change-functions #'jb-kotlin--conflict-file-changed)
(add-hook 'window-configuration-change-hook #'jb-kotlin--conflict-windows-changed)
(add-hook 'lsp-after-uninitialized-functions #'jb-kotlin--refactor-disconnected)

(provide 'jb-kotlin-refactor)
;;; jb-kotlin-refactor.el ends here

;;; jb-kotlin-terminal.el --- Interactive Kotlin debug consoles -*- lexical-binding: t; -*-
;;; Commentary:
;; Preserve DAP argv/environment and provide stdin through comint.
;;; Code:
(require 'dap-mode)
(require 'comint)

(defvar jb-kotlin--terminal-processes (make-hash-table :test 'eq)
  "Terminal processes owned by each Kotlin debug session.")

(defun jb-kotlin--dap-terminal (original session request)
  "Handle Kotlin SESSION's terminal REQUEST, otherwise call ORIGINAL."
  (if (not (member (plist-get (dap--debug-session-launch-args session) :type)
                   '("intellij_jvm" "intellij_gradle" "intellij_debugger")))
      (funcall original session request)
    (condition-case err
        (let* ((args (gethash "arguments" request))
               (kind (or (gethash "kind" args) dap-default-terminal-kind))
               (environment (gethash "env" args))
               (process-environment (copy-sequence process-environment)))
          (when environment
            (unless (hash-table-p environment) (error "Invalid terminal environment"))
            (maphash (lambda (key value)
                       (unless (and (stringp key) (or (null value) (stringp value)))
                         (error "Invalid terminal environment entry"))
                       (setenv key value)) environment))
          (pcase kind
            ("external" (funcall original session request))
            ("integrated"
             (let ((argv (append (gethash "args" args) nil))
                   (default-directory (file-name-as-directory (gethash "cwd" args))))
               (unless (and argv (cl-every #'stringp argv)) (error "Invalid terminal arguments"))
               (when (eq t (gethash "argsCanBeInterpretedByShell" args))
                 (error "Shell-interpreted terminal arguments are unsupported"))
               (let ((buffer (dap--make-terminal-buffer (gethash "title" args) session)))
                 (apply #'make-comint-in-buffer "Kotlin debug" buffer (car argv) nil (cdr argv))
                 (with-current-buffer buffer (comint-mode))
                 (let ((process (get-buffer-process buffer)))
                   (set-process-query-on-exit-flag process nil)
                   (push process (gethash session jb-kotlin--terminal-processes))
                   (display-buffer buffer)
                   (dap--send-message
                    (dap--make-success-response (gethash "seq" request) "runInTerminal"
                                                (list :processId (process-id process)))
                    (dap--resp-handler) session)))))
            (_ (error "Unknown terminal kind: %s" kind))))
      (error
       (dap--send-message (dap--make-error-response (gethash "seq" request) "runInTerminal" nil
                                                   (error-message-string err))
                          (dap--resp-handler) session)))))

(defun jb-kotlin--terminal-cleanup (session)
  "Stop terminal processes owned by terminated SESSION."
  (dolist (process (gethash session jb-kotlin--terminal-processes))
    (when (process-live-p process) (delete-process process)))
  (remhash session jb-kotlin--terminal-processes))

(advice-add 'dap--start-process :around #'jb-kotlin--dap-terminal)
(add-hook 'dap-terminated-hook #'jb-kotlin--terminal-cleanup)
(provide 'jb-kotlin-terminal)
;;; jb-kotlin-terminal.el ends here

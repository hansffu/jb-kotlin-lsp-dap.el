;;; init.el --- Standalone development Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded by `just run', in a new Emacs process without the user's init file.
;; Installed packages and tree-sitter grammars remain available.

;;; Code:

(require 'package)
(unless (and (locate-library "lsp-mode") (locate-library "dap-mode")
             (or (locate-library "kotlin-mode") (locate-library "kotlin-ts-mode")))
  (package-initialize))
(setq load-prefer-newer t)

(defvar jb-kotlin-dev-state-directory
  (file-name-as-directory (make-temp-file "jb-kotlin-emacs-" t))
  "Private state and server logs for this development Emacs instance.")

(setq custom-file (expand-file-name "custom.el" jb-kotlin-dev-state-directory)
      lsp-session-file (expand-file-name "lsp-session" jb-kotlin-dev-state-directory)
      dap-breakpoints-file (expand-file-name "breakpoints" jb-kotlin-dev-state-directory)
      dap-ui-repl-history-dir jb-kotlin-dev-state-directory)

(require 'jb-kotlin-lsp-dap)
(require 'lsp-lens)
(require 'lsp-modeline)
(require 'lsp-headerline)
(require 'lsp-diagnostics)
(require 'lsp-completion)
(require 'dap-ui)
(require 'dap-mouse)

(setq jb-kotlin-server-command
      (list "kotlin-lsp" "--stdio"
            (concat "--system-path="
                    (expand-file-name "system" jb-kotlin-dev-state-directory)))
      lsp-enabled-clients '(jb-kotlin)
      lsp-enable-snippet nil
      lsp-lens-enable t)

(setenv "IJ_JAVA_OPTIONS"
        (concat (getenv "IJ_JAVA_OPTIONS")
                " -Didea.config.path=" (expand-file-name "config" jb-kotlin-dev-state-directory)
                " -Didea.log.path=" (expand-file-name "log" jb-kotlin-dev-state-directory)
                " -Djava.util.prefs.userRoot=" (expand-file-name "prefs" jb-kotlin-dev-state-directory)))

(cond
 ((and (require 'kotlin-ts-mode nil t)
       (fboundp 'treesit-ready-p)
       (treesit-ready-p 'kotlin t))
  (add-to-list 'auto-mode-alist '("\\.kts?\\'" . kotlin-ts-mode)))
 ((require 'kotlin-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.kts?\\'" . kotlin-mode)))
 (t (display-warning 'jb-kotlin
                     "Install kotlin-mode or kotlin-ts-mode with its Kotlin grammar to edit Kotlin files.")))

(add-hook 'kotlin-mode-hook #'lsp-deferred)
(add-hook 'kotlin-ts-mode-hook #'lsp-deferred)
(dap-auto-configure-mode 1)

(setq initial-scratch-message
      (concat ";; JetBrains Kotlin development instance: this checkout is loaded.\n"
              ";; Open a Kotlin project file with C-x C-f to start LSP.\n"
              ";; M-x jb-kotlin-debug launches a main class.\n"
              ";; Private state and logs: " jb-kotlin-dev-state-directory "\n\n"))
(message "Kotlin development Emacs; private state: %s" jb-kotlin-dev-state-directory)

;;; init.el ends here

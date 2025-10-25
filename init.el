;;; init.el --- My Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Modular Emacs configuration
;; Early initialization is in early-init.el
;; Individual modules are in lisp/ directory

;;; Code:

;;;; Add lisp directory to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;;; Load configuration modules
;; Load modules in the correct order to handle dependencies

;; Core settings and package manager
(require 'my-core)

;; Custom utility functions
(require 'my-defuns)

;; Editing enhancements
(require 'my-editing)

;; User interface
(require 'my-ui)

;; Window management
(require 'my-windows)

;; Completion framework
(require 'my-completion)

;; Help and documentation
(require 'my-help)

;; Org mode
(require 'my-org)

;; TRAMP
(require 'my-tramp)

;; Programming
(require 'my-programming)

;; Terminal
(require 'my-terminal)

;; File management
(require 'my-files)

;; Search tools
(require 'my-search)

;; Miscellaneous packages
(require 'my-misc)

;; AI tooling
(require 'my-ai)

;;;; Work-specific configuration
(let ((work-config-file (expand-file-name "work-config.el" user-emacs-directory)))
  (when (file-exists-p work-config-file)
    (condition-case err
        (load work-config-file)
      (error (message "Failed to load work config: %s" err)))))

;;;; Macros
(require 'kmacro)
(defalias 'kmacro-insert-macro 'insert-kbd-macro)
(define-key kmacro-keymap (kbd "I") #'kmacro-insert-macro)

;;;; Additional AI packages
(use-package shell-maker
  :straight (shell-maker :type git :host github :repo "xenodium/shell-maker"))

(use-package acp
  :straight (acp :type git :host github :repo "xenodium/acp.el" :files ("*.el")))

(use-package agent-shell
  :straight (agent-shell :type git :host github :repo "xenodium/agent-shell")
  :config
  (setq agent-shell-anthropic-authentication
      (agent-shell-anthropic-make-authentication :login t)))

;;;; Startup finalization

;; Reset GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)) ; 16MB
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(provide 'init)
;;; init.el ends here

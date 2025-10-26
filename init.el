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

;; Org mode (split into focused modules)
(require 'my-org-core)
(require 'my-org-capture)
(require 'my-org-agenda)
(require 'my-org-babel)

;; TRAMP
(require 'my-tramp)

;; Programming (split into focused modules)
(require 'my-prog-core)
(require 'my-prog-git)
(require 'my-prog-lsp)
(require 'my-prog-langs)

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

;; Work configs
(require 'my-work)

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

;; Run garbage collection when Emacs has been idle for 5 minutes
;; Only run if still idle when timer fires to avoid stuttering
;; This prevents GC pauses during active editing by running cleanup during natural breaks
(run-with-idle-timer 300 t
  (lambda ()
    (when (current-idle-time)
      (garbage-collect))))

(provide 'init)
;;; init.el ends here

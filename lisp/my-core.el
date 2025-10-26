;;; my-core.el --- Core Emacs settings -*- lexical-binding: t -*-

;;; Commentary:
;; Essential Emacs settings and paths

;;; Code:

;;;; Emacs directory
(setq user-emacs-directory (expand-file-name "~/.emacs.d"))

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;; Disable creating backup and lock files
(setq make-backup-files nil)
(setq backup-inhibited nil)
(setq create-lockfiles nil)

;;;; Start emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;;;; Default coding system
(set-default-coding-systems 'utf-8)

;;;; Prevent Emacs from closing by accident
(setq confirm-kill-emacs 'y-or-n-p)

;;;; Disable prompts
(setq use-short-answers t)
(setq confirm-nonexistent-file-or-buffer nil)

;; Don't ask when killing a buffer with a live process attached to it
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;;;; Package manager (straight.el)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq use-package-always-ensure nil)
(setq straight-use-package-version 'straight)
(setq straight-use-package-by-default t)

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)
(straight-use-package 'org)

;; FIX: needed to avoid this error: https://github.com/radian-software/straight.el/issues/1146#issuecomment-1949645571
(straight-use-package 'project)

;;;; Organize files

;; Keep folders clean
(setq url-history-file (expand-file-name "url/history" user-emacs-directory))

(use-package no-littering)

;; auto-save-mode doesn't create the path automatically
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(provide 'my-core)
;;; my-core.el ends here

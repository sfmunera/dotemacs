;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;;; Commentary:
;; Emacs 27+ loads this file before init.el, before GUI is initialized,
;; and before package.el is loaded. This is the optimal place for
;; performance optimizations and early UI setup.

;;; Code:

;;;; Performance Optimizations

;; Increase garbage collection threshold during startup
;; This will be reset in init.el after startup completes
(setq gc-cons-threshold most-positive-fixnum)

;; MacOS specific library path
(when (eq system-type 'darwin)
  (setenv "LIBRARY_PATH"
          (concat (getenv "LIBRARY_PATH")
                  ":/opt/homebrew/lib/gcc/current")))

;;;; Native Compilation

;; Make native compilation silent and prune its cache
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-compile-prune-cache t))

;;;; UI Optimizations

;; Disable UI elements early to prevent flashing on startup
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq visible-bell t)

;; Don't show startup screen
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;;;; Package System

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;;; early-init.el ends here

;;; my-org-core.el --- Core Org-mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Core org-mode settings, styling, and helper functions

;;; Code:

;;;; Core Org Mode Setup

;; Org mode configuration
(defun my/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . my/org-mode-setup)
  :config
  ;; Custom org mode settings
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis " ▾"
   org-startup-folded 'content

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────"

   ;; Code blocks config
   org-src-fontify-natively t
   org-src-tab-acts-natively t
   org-edit-src-content-indentation 2

   ;; Org files
   org-directory "~/Org/"
   org-default-notes-file "daily.org"
   org-agenda-files (list "~/Org/")
   org-startup-with-inline-images t

   ;; Tags, TODO keywords
   org-log-done 'time
   org-log-into-drawer t
   org-use-fast-todo-selection t
   org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "WAITING(w)" "FOLLOW-UP(f)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; set C-a and C-e explicitly because back-to-indentation-or-beginning conflicts with org-special-ctrl-a/e
  (define-key org-mode-map "\C-a" 'org-beginning-of-line)
  (define-key org-mode-map "\C-e" 'org-end-of-line)

  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 10)) (nil . (:maxlevel . 10))))
  (setq org-refile-use-outline-path 'file)
  ;; makes org-refile outline working with completion framework
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way.
  ;; Everything else will be variable-pitch
  (set-face-attribute 'org-block nil :foreground 'unspecified :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  ;(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-formula nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;;;; Helper Functions

(defun my/org-days-between (start-date end-date)
  "Calculate days between two org timestamps"
  (when (and start-date end-date)
    (let ((start (org-time-string-to-time start-date))
          (end (org-time-string-to-time end-date)))
      (floor (/ (float-time (time-subtract end start))
                86400)))))

(defun my/org-get-year (timestamp)
  "Extract year from org timestamp"
  (when timestamp
    (format-time-string "%Y"
                        (org-time-string-to-time timestamp))))

(defun my/calculate-reading-days ()
  "Calculate and store the reading duration when a book is marked as DONE"
  (let* ((start-date (org-entry-get nil "STARTED"))
         (end-date (org-entry-get nil "FINISHED"))
         (days (my/org-days-between start-date end-date)))
    (when days
      (org-entry-put nil "DAYS_TO_READ" (number-to-string (+ 1 days))))))

(defun my/org-book-state-change ()
  "Handle book state changes"
  (cond
   ((string= org-state "READING")
    (unless (org-entry-get nil "STARTED")  ; Only if STARTED not already set
      (let ((today (format-time-string "[%Y-%m-%d %a]")))
        (org-entry-put nil "STARTED" today)
        ;; Set YEAR_READ to current year when starting
        (org-entry-put nil "YEAR_READ" (my/org-get-year today)))))
   ((string= org-state "READ")
    (let ((today (format-time-string "[%Y-%m-%d %a]")))
      (org-entry-put nil "FINISHED" today)
      ;; Update YEAR_READ to completion year
      (org-entry-put nil "YEAR_READ" (my/org-get-year today))
      (my/calculate-reading-days)
      (org-set-property "RATING" (read-string "Rating (1-5): "))))))

;; Add the hook
(add-hook 'org-after-todo-state-change-hook #'my/org-book-state-change)

;;;; Styling Packages

(use-package org-modern
  :after org
  :custom-face
  (org-modern-label
   ((t :height 0.8 :width condensed :weight regular
       :underline nil :inherit fixed-pitch)))
  :config
  (global-org-modern-mode))

(require 'org-indent)

(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-timeblock
  :straight (org-timeblock :type git
              :host github
              :repo "ichernyshovvv/org-timeblock")
  :after org
  :config
  (setq org-timeblock-inbox-file (expand-file-name "daily.org" org-directory)
        org-timeblock-show-outline-path t
        org-timeblock-span 1
        org-timeblock-scale-options '(8 . 18)))

;;;; Table of Contents

;; Update table of contents on save
(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

(provide 'my-org-core)
;;; my-org-core.el ends here

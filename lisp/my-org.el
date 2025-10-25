;;; my-org.el --- Org-mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Org-mode configuration

;;; Code:

;;; Org mode

;; Org mode configuration
(defun my/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . my/org-mode-setup)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
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
   '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "WAITING(w)" "FOLLOW-UP(f)" "|" "DONE(d)" "CANCELLED(c)"))

   ;; Capture configurations
   org-capture-templates
   `(("b" "Book" entry
      (file+headline "Books.org" "Books")
      ,(mapconcat
        #'identity
        '("*** TO-READ %^{Title}"
          "    :PROPERTIES:"
          "    :ADDED: %U"
          "    :AUTHOR: %^{Author}"
          "    :CATEGORY: %^{Category|Technical|Non-Technical}"
          "    :RATING:"
          "    :DAYS_TO_READ:"
          "    :END:")
        "\n"))
     ("w" "Work")
     ("ww" "Weekly Plan" plain
      (file+olp+datetree "plans.org" "Weekly Plan")
      ,(mapconcat
        #'identity
        '("%?"
          ":PROPERTIES:"
          ":CAPTURED: %U"
          ":END:")
        "\n")
      :empty-lines 1
      :prepend t
      :tree-type week)
     ("wd" "Daily Entry" entry (file "daily.org") "* %<%Y-%m-%d %A>\n%?")
     ("p" "Personal")
     ("pi" "Inbox Capture" entry (file+headline "Inbox.org" "Tasks")
      "* TODO %?\n %U\n" :empty-lines 1 :kill-buffer t)
     ("pj" "Journal" entry (file+olp+datetree "Journal.org")
      "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
      :clock-in :clock-resume :empty-lines 1)))

  ;; set C-a and C-e explicitly because back-to-indentation-or-beginning conflicts with org-special-ctrl-a/e
  (define-key org-mode-map "\C-a" 'org-beginning-of-line)
  (define-key org-mode-map "\C-e" 'org-end-of-line)

  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 10)) (nil . (:maxlevel . 10))))
  (setq org-refile-use-outline-path 'file)
  ;; makes org-refile outline working with completion framework
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way.
  ;; Eveything else will be variable-pitch
  (set-face-attribute 'org-block nil :foreground 'unspecified :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
                                        ;(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-formula nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

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

(setq org-agenda-custom-commands
      '(("r" "Reading List Overview"
           ((tags "CATEGORY=\"Technical\"|CATEGORY=\"Non-Technical\""
                     ((org-agenda-files '("Books.org"))
                      (org-agenda-prefix-format "  %-12c: ")
                      (org-super-agenda-groups
                       '((:name "Currently Reading"
                          :todo "READING"
                          :order 1)
                         (:name "Technical Books To Read"
                          :and (:todo "TO-READ"
                                :category "Technical")
                          :order 2)
                         (:name "Non-Technical Books To Read"
                          :and (:todo "TO-READ"
                                :category "Non-Technical")
                          :order 3)
                         (:name "Recently Completed Technical Books"
                          :and (:todo "READ"
                                :category "Technical"
                                )
                          :order 4)
                         (:name "Recently Completed Non-Technical Books"
                          :and (:todo "READ"
                                :category "Non-Technical"
                                )
                          :order 5)
                         (:discard (:anything t))))))))
        ("h" . "House Manual Views")
        ("hd" "🏠 House Dashboard"
         ((tags-todo "urgent"
                     ((org-agenda-overriding-header "🚨 URGENT TASKS")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-prefix-format "  %-12c: ")
                      (org-agenda-remove-tags t)
                      (org-agenda-sorting-strategy '(priority-down))
                      ))
          (tags-todo "repair"
                     ((org-agenda-overriding-header "🔧 REPAIRS NEEDED")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-prefix-format "  %-12c: ")
                      (org-agenda-tag-filter-preset '("+repair"))
                      (org-agenda-sorting-strategy '(priority-down))))
          (tags-todo "improvement"
                     ((org-agenda-overriding-header "⬆️  IMPROVEMENTS & UPGRADES")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-prefix-format "  %-12c: ")
                      (org-agenda-tag-filter-preset '("+improvement"))
                      (org-agenda-sorting-strategy '(priority-down))))
          (agenda ""
                  ((org-agenda-overriding-header "📅 THIS WEEK'S MAINTENANCE")
                   (org-agenda-span 7)
                   (org-agenda-start-on-weekday nil)
                   (org-agenda-start-day "today")
                   (org-agenda-tag-filter-preset '("+maintenance"))
                   (org-agenda-prefix-format "  %t: %-12c ")
                   (org-agenda-todo-keyword-format ""))))
         ((org-agenda-files '("Notes/Personal/House.org"))
          (org-agenda-compact-blocks t)
          (org-agenda-block-separator ?─)))
        
        ;; Weekly Planning View
        ("hw" "📅 Weekly Maintenance Plan"
         ((agenda ""
                  ((org-agenda-overriding-header "🗓️  NEXT 7 DAYS")
                   (org-agenda-span 7)
                   (org-agenda-start-on-weekday nil)
                   (org-agenda-start-day "today")
                   (org-agenda-tag-filter-preset '("+maintenance"))
                   (org-agenda-prefix-format "  %t: %-15c %s")
                   (org-agenda-todo-keyword-format "")
                   (org-agenda-scheduled-leaders '("📋 " "📋 "))
                   (org-agenda-time-grid nil))))
         ((org-agenda-files '("Notes/Personal/House.org"))
          (org-agenda-compact-blocks t)))
        
        ;; Monthly Planning View
        ("hm" "📆 Monthly Maintenance Plan"
         ((agenda ""
                  ((org-agenda-overriding-header "🗓️  NEXT 30 DAYS")
                   (org-agenda-span 30)
                   (org-agenda-start-on-weekday nil)
                   (org-agenda-start-day "today")
                   (org-agenda-tag-filter-preset '("+maintenance"))
                   (org-agenda-prefix-format "  %t: %-15c %s")
                   (org-agenda-todo-keyword-format "")
                   (org-agenda-scheduled-leaders '("📋 " "📋 "))
                   (org-agenda-time-grid nil))))
         ((org-agenda-files '("Notes/Personal/House.org"))
          (org-agenda-compact-blocks t)))
        
        ;; Quarterly Planning View
        ("hq" "📊 Quarterly Maintenance Plan"
         ((agenda ""
                  ((org-agenda-overriding-header "🗓️  NEXT 90 DAYS")
                   (org-agenda-span 90)
                   (org-agenda-start-on-weekday nil)
                   (org-agenda-start-day "today")
                   (org-agenda-tag-filter-preset '("+maintenance"))
                   (org-agenda-prefix-format "  %t: %-15c %s")
                   (org-agenda-todo-keyword-format "")
                   (org-agenda-scheduled-leaders '("📋 " "📋 "))
                   (org-agenda-time-grid nil)
                   (org-agenda-show-all-dates nil))))
         ((org-agenda-files '("Notes/Personal/House.org"))
          (org-agenda-compact-blocks t)))
        
        ;; Seasonal View
        ("hs" "🌅 Seasonal Maintenance"
         ((tags-todo "seasonal"
                     ((org-agenda-overriding-header "🌱 SEASONAL TASKS")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-prefix-format "  %-15c: %s")
                      (org-agenda-sorting-strategy '(priority-down)))))
         ((org-agenda-files '("Notes/Personal/House.org"))
          (org-agenda-compact-blocks t)))
        
        ;; Cost Planning View
        ("hc" "💰 Cost Planning"
         ((tags-todo "+TODO=\"TODO\"+COST>0"
                     ((org-agenda-overriding-header "💸 TASKS WITH COSTS")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-prefix-format "  $%-4(org-entry-get nil \"COST\"): %-15c %s")
                      (org-agenda-sorting-strategy '(user-defined-down))))
          
          (tags-todo "urgent+COST>0"
                     ((org-agenda-overriding-header "🚨 URGENT TASKS WITH COSTS")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-prefix-format "  $%-4(org-entry-get nil \"COST\"): %-15c %s"))))
         ((org-agenda-files '("Notes/Personal/House.org"))
          (org-agenda-compact-blocks t)))
        
        ;; All Repairs & Improvements
        ("hr" "🔧 All Repairs & Improvements"
         ((tags-todo "repair+TODO=\"TODO\""
                     ((org-agenda-overriding-header "🔧 ALL REPAIRS")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-prefix-format "  %-15c: %s")
                      (org-agenda-sorting-strategy '(priority-down))))
          
          (tags-todo "improvement+TODO=\"TODO\""
                     ((org-agenda-overriding-header "⬆️  ALL IMPROVEMENTS")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-prefix-format "  %-15c: %s")
                      (org-agenda-sorting-strategy '(priority-down)))))
         ((org-agenda-files '("Notes/Personal/House.org"))
          (org-agenda-compact-blocks t)))

        ("w" "Work Projects and Tasks Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "🗓️ Today"
                                :time-grid t
                                :date today
                                :todo "TODAY"
                                :scheduled today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "\n\n Work Tasks Overview \n━━━━━━━━━━━━━━━━━━━━━━━━━")
                       (org-super-agenda-groups
                        '((:name "⭐ Important Tasks"
                                 :priority "A"
                                 :order 1)
                          (:name "🔥 Active Tasks"
                                 :todo "IN-PROGRESS"
                                 :order 2)
                          (:name "➡️ Ready Tasks"
                                 :todo "NEXT"
                                 :order 3)
                          (:name "Follow Up"
                                 :todo ("FOLLOW-UP")
                                 :order 4)
                          (:name "Tasks waiting for something"
                                 :todo "WAITING"
                                 :order 5)
                          (:name "Nice to Have Tasks"
                                 :priority "B"
                                 :order 6)
                          (:name "📁 Task Backlog"
                                 :todo "TODO"
                                 :order 7)
                          )))))
         ((org-agenda-files (list "~/Org/"))
          (org-agenda-compact-blocks t)))

        ("p" "Personal Projects and Tasks Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "🗓️ Today"
                                :time-grid t
                                :date today
                                :todo "TODAY"
                                :scheduled today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "\n\n✨ PROJECTS ✨\n━━━━━━━━━━━━━━━━━━━━━━━━━")
                       (org-super-agenda-groups
                        '((:discard (:not (:tag "project")))
                          (:name "📦 Active Projects"
                           :todo "ACTIVE"
                           :order 1)
                          (:name "📅 Project Backlog"
                           :todo "BACKLOG"
                           :order 2)
                          (:name "🔥 Active Tasks"
                                 :todo "IN-PROGRESS"
                                 :order 3)
                          (:name "➡️ Next Tasks"
                                 :todo "NEXT"
                                 :order 4)
                          (:name "📋 Task Backlog"
                                 :todo "TODO"
                                 :order 5)
                          ))))
          (alltodo "" ((org-agenda-overriding-header "\n\n✨ GENERAL TASKS ✨\n━━━━━━━━━━━━━━━━━━━━━━━━━")
                       (org-super-agenda-groups
                        '((:discard (:tag "project"))
                          (:name "⭐ Important Tasks"
                                 :priority "A"
                                 :order 1)
                          (:name "🔥 Active Tasks"
                                 :todo "IN-PROGRESS"
                                 :order 2)
                          (:name "➡️ Next Tasks"
                                 :todo "NEXT"
                                 :order 3)
                          (:name "📁 Backlog"
                                 :todo "TODO"
                                 :order 4)
                          (:name "➕ Other Tasks"
                           :auto-category t
                           :order 5))))))
         ((org-agenda-files '("Projects.org" "phone/Inbox.org"))
          (org-agenda-compact-blocks t)))))

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

(use-package jupyter
  :after (org))

;; Org babel
;; TODO: Organize into org-mode config
(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)))
;;(org-babel-jupyter-override-src-block "python")


(require 'ob-python)

(setq org-confirm-babel-evaluate nil)

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; shortcut for source blocks: e.g. <el TAB autocompletes the source block
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("rs" . "src rust"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

;; Update table of contents on save
(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

;; My agenda should show me at a glance everything I need to know for the day
;; - Top priority tasks: anything that is marked as "drop everything until this is done"
;; - Next items: what is next to be able to make progress on projects
;; - Waiting: things that I need to follow up on or I'm waiting on
;; - Figure out: Tasks that need to be clarified
;; - Admin/short tasks: tasks that are low effort and can be batched
;; - Learn: Things to learn

;; TODO: Finish setting up my agenda
(use-package org-super-agenda
  :config
  (org-super-agenda-mode))
(setq org-super-agenda-groups
      '((:name "Top Priority"
               :priority "A"
               :order 1)
        (:name "Started"
               :todo ("STARTED")
               :order 2)
        (:name "To Clarify" ;; When not sure what to do yet, needs clarification before READY
               :todo ("CLARIFY")
               :order 3)
        (:name "To Discuss" ;; To discuss during a meeting
               :todo ("TO-DISCUSS")
               :order 4)
        (:name "Follow Up" ;; Follow up on something that doesn't depend on me
               :todo ("FOLLOW-UP")
               :order 5)
        (:name "Waiting" ;; Waiting to hear back from someone
               :todo ("WAITING")
               :order 6)
        (:name "Ready" ;; Ready to start working on
               :todo ("READY")
               :order 7)
        (:name "Scheduled" ;;
               :todo ("SCHEDULED")
               :order 8)
        (:name "On Hold" ;; Temporarily paused or holding on something external
               :todo ("ON-HOLD")
               :order 9)
        (:name "Backburner" ;; Important but not planning to work on yet
               :todo ("BACKBURNER")
               :order 10)
        ))

(provide 'my-org)
;;; my-org.el ends here

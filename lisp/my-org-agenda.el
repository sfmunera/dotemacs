;;; my-org-agenda.el --- Org agenda configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Org agenda custom commands and super-agenda configuration
;;
;; My agenda should show me at a glance everything I need to know for the day:
;; - Top priority tasks: anything that is marked as "drop everything until this is done"
;; - Next items: what is next to be able to make progress on projects
;; - Waiting: things that I need to follow up on or I'm waiting on
;; - Figure out: Tasks that need to be clarified
;; - Admin/short tasks: tasks that are low effort and can be batched
;; - Learn: Things to learn

;;; Code:

(require 'org)

;;;; Helpers

(defun my/org-agenda-item-date ()
  "Return scheduled or deadline date string for use in agenda prefix."
  (let* ((stamp (or (org-entry-get nil "SCHEDULED")
                    (org-entry-get nil "DEADLINE"))))
    (format "%-14s"
            (if stamp
                (format-time-string "%b %d %Y" (org-time-string-to-time stamp))
              ""))))

;;;; Agenda Custom Commands

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
        ("h" "🏠 House"
         ((alltodo ""
                   ((org-agenda-todo-keyword-format "")
                    (org-agenda-sorting-strategy '(scheduled-up deadline-up))
                    (org-agenda-prefix-format "  %-12c %(my/org-agenda-item-date)")
                    (org-super-agenda-groups
                     '((:name "🚨 Urgent"
                        :tag "urgent"
                        :order 1)
                       (:name "⏰ Overdue"
                        :scheduled past
                        :deadline past
                        :order 2)
                       (:name "📅 Today"
                        :scheduled today
                        :deadline today
                        :order 3)
                       (:name "📆 This Week"
                        :pred (lambda (item)
                                (when-let* ((m (or (get-text-property 1 'org-marker item)
                                                   (get-text-property 1 'org-hd-marker item)))
                                            (stamp (or (org-entry-get m "SCHEDULED")
                                                       (org-entry-get m "DEADLINE"))))
                                  (let* ((abs (org-time-string-to-absolute stamp))
                                         (today (org-today)))
                                    (and (> abs today) (<= abs (+ today 7))))))
                        :order 4)
                       (:name "🗓️ This Month"
                        :pred (lambda (item)
                                (when-let* ((m (or (get-text-property 1 'org-marker item)
                                                   (get-text-property 1 'org-hd-marker item)))
                                            (stamp (or (org-entry-get m "SCHEDULED")
                                                       (org-entry-get m "DEADLINE"))))
                                  (let* ((abs (org-time-string-to-absolute stamp))
                                         (today (org-today)))
                                    (and (> abs (+ today 7)) (<= abs (+ today 30))))))
                        :order 5)
                       (:name "🛠️ Improvements"
                        :tag "improvement"
                        :order 6)
                       (:discard (:anything t)))))))
         ((org-agenda-files '("Notes/Personal/House.org"))
          (org-agenda-compact-blocks t)
          (org-agenda-block-separator ?─)))

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

;;;; Super Agenda Configuration

;; TODO: Finish setting up my agenda
(use-package org-super-agenda
  :hook (after-init . org-super-agenda-mode))

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

;;;; Key Bindings

(global-set-key (kbd "C-c a") 'org-agenda)

(provide 'my-org-agenda)
;;; my-org-agenda.el ends here

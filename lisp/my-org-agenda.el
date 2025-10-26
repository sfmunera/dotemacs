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

;;;; Super Agenda Configuration

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

;;;; Key Bindings

(global-set-key (kbd "C-c a") 'org-agenda)

(provide 'my-org-agenda)
;;; my-org-agenda.el ends here

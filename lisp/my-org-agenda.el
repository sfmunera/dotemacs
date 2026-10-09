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

(defun my/org-agenda-process-block (&optional _match)
  "Agenda block listing untriaged #process captures in daily.org.
The built-in agenda only lists headings; captures are list items, so scan
for lines ending in #process and link each one back to its source."
  (org-agenda-prepare "#process")
  (let ((file (expand-file-name "daily.org" org-directory))
        (inhibit-read-only t)
        items)
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       ;; End-of-line anchor skips the template's "triage #process, ..."
       ;; and "#process at zero:" lines, and the Hi-lock config.
       (while (re-search-forward "#process[ \t]*$" nil t)
         (let ((marker (copy-marker (line-beginning-position)))
               (text (string-trim
                      (replace-regexp-in-string
                       "^[ \t]*- \\|[ \t]*#process[ \t]*$" ""
                       (buffer-substring (line-beginning-position) (line-end-position)))))
               (day (save-excursion
                      (when (re-search-backward "^\\*\\*\\* \\([0-9]\\{4\\}-[0-9-]+ [A-Za-z]+\\)" nil t)
                        (match-string-no-properties 1)))))
           (push (list marker (org-link-display-format text) day) items)))))
    (insert (propertize (format "📥 Untriaged #process (%d)\n" (length items))
                        'face 'org-agenda-structure))
    (if (null items)
        (insert "  Inbox zero\n")
      (dolist (item (nreverse items))
        (pcase-let ((`(,marker ,text ,day) item))
          (insert (propertize (format "  %-16s %s\n" (or day "") text)
                              'org-marker marker
                              'org-hd-marker marker
                              'mouse-face 'highlight)))))))

(defun my/org-agenda-people-waiting-block (&optional _match)
  "Agenda block listing open `Waiting on:' items under * People in daily.org.
Items are `- [YYYY-MM-DD] text' lines; received ones move to History, so
everything listed here is still open. Oldest first; over a week is flagged."
  (org-agenda-prepare "Waiting on")
  (let ((file (expand-file-name "daily.org" org-directory))
        (inhibit-read-only t)
        items)
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (when (re-search-forward "^\\* People$" nil t)
         (let ((end (save-excursion (org-end-of-subtree t t))))
           (while (re-search-forward "^\\*\\* \\(.+\\)$" end t)
             (let ((person (match-string-no-properties 1))
                   (person-end (save-excursion (org-end-of-subtree t t))))
               (when (re-search-forward "^Waiting on:$" person-end t)
                 (forward-line 1)
                 ;; Every "-" line until the next label; bare "-" placeholders
                 ;; are skipped rather than ending the list.
                 (while (looking-at "^-\\(?:[ \t]+\\(.*\\)\\)?$")
                   (let ((body (string-trim (or (match-string-no-properties 1) "")))
                         date)
                     (when (string-match "^\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\][ \t]*" body)
                       (setq date (match-string 1 body)
                             body (substring body (match-end 0))))
                     (unless (string-empty-p body)
                       (push (list (copy-marker (point)) person date
                                   (org-link-display-format body))
                             items)))
                   (forward-line 1)))
               (goto-char person-end)))))))
    ;; Undated items sort first: they're the ones nobody knows the age of.
    (setq items (sort items (lambda (a b) (string< (or (nth 2 a) "") (or (nth 2 b) "")))))
    (insert (propertize (format "🤝 Waiting on (People) (%d)\n" (length items))
                        'face 'org-agenda-structure))
    (if (null items)
        (insert "  Nothing outstanding\n")
      (dolist (item items)
        (pcase-let* ((`(,marker ,person ,date ,text) item)
                     (age (when date
                            (- (org-today) (org-time-string-to-absolute date)))))
          (insert (propertize (format "  %-10s %-11s %-60s %s\n"
                                      person (or date "") (truncate-string-to-width text 60)
                                      (if age (format "%dd" age) ""))
                              'face (when (and age (> age 7)) 'org-warning)
                              'org-marker marker
                              'org-hd-marker marker
                              'mouse-face 'highlight)))))))

;;;; Agenda Custom Commands

(setq org-agenda-custom-commands
      '(("d" "Daily dashboard"
         ;; Old day sections have keyword-less meeting headings with past
         ;; SCHEDULED stamps; without the skip they flood the day view.
         ((agenda "" ((org-agenda-span 'day)
                      (org-agenda-skip-function
                       '(org-agenda-skip-entry-if 'nottodo 'any))))
          (todo "WAITING|FOLLOW-UP"
                ((org-agenda-overriding-header "⏳ Waiting on someone / follow up")
                 (org-agenda-prefix-format "  %(my/org-agenda-item-date)")))
          (my/org-agenda-people-waiting-block)
          (todo "IN-PROGRESS|NEXT"
                ((org-agenda-overriding-header "➡️ In progress / next")
                 (org-agenda-prefix-format "  ")))
          (my/org-agenda-process-block))
         ((org-agenda-files '("~/Org/daily.org"))
          (org-super-agenda-groups nil)
          (org-agenda-compact-blocks t)))

        ("r" "Reading List Overview"
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

;;; my-org-capture.el --- Org capture templates -*- lexical-binding: t -*-

;;; Commentary:
;; Org capture template configurations

;;; Code:

(require 'org)

;;;; Capture Templates

(defun my/org-find-daily-entry ()
  "Navigate to today's entry in daily log file using existing heading format."
  (let* ((week (format-time-string "%G-W%V"))
         (day (format-time-string "%Y-%m-%d %a")))
    (goto-char (point-min))
    (unless (search-forward (concat "** " week) nil t)
      (user-error "No entry for week %s — capture this week's entry first" week))
    (unless (search-forward (concat "*** " day) nil t)
      (user-error "No entry for %s — capture this week's entry first" day))
    (let ((day-end (save-excursion (org-end-of-subtree t t))))
      (unless (re-search-forward "^\\*\\*\\*\\* Log$" day-end t)
        (user-error "No Log heading under %s" day)))))

(defvar my/capture-week-start nil
  "The Monday date used for the current weekly capture.")

(defun my/set-capture-week-start ()
  "Prompt for the Monday to use for weekly capture."
  (unless my/capture-week-start
    (let* ((dow (string-to-number (format-time-string "%u")))
           (days-ahead (mod (- 8 dow) 7))
           (default-monday (time-add (current-time) (days-to-time days-ahead)))
           (input (org-read-date nil t nil "Week starting Monday: " default-monday)))
      (setq my/capture-week-start input))))

(defun my/reset-capture-week-start ()
  "Reset week start after capture completes."
  (setq my/capture-week-start nil))

(add-hook 'org-capture-after-finalize-hook #'my/reset-capture-week-start)

(defun my/week-id ()
  "Return the ISO week string for the capture week."
  (my/set-capture-week-start)
  (format-time-string "%G-W%V" my/capture-week-start))

(defun my/week-day (n)
  "Return date string for day N of the capture week (0=Mon, 4=Fri)."
  (my/set-capture-week-start)
  (format-time-string "%Y-%m-%d %a"
    (time-add my/capture-week-start (days-to-time n))))

(defvar my/weekly-day-template "~/.emacs.d/templates/weekly-day.org"
  "File holding the shared body for a single day in the weekly template.")

(defun my/week-day-block (n)
  "Return the dated day scaffold for day N (0=Mon) of the capture week."
  (concat "*** " (my/week-day n) "\n"
          (with-temp-buffer
            (insert-file-contents my/weekly-day-template)
            (buffer-string))))

(defun my/week-days ()
  "Return the Monday-Friday scaffolds for the capture week."
  (mapconcat #'my/week-day-block (number-sequence 0 4) "\n"))

(defun my/org-find-weekly-insertion-point ()
  "Navigate to insertion point for next week's entry.
     Finds or creates the correct quarterly heading and appends there."
  (let* ((year (string-to-number (format-time-string "%G" my/capture-week-start)))
         (month (string-to-number (format-time-string "%m" my/capture-week-start)))
         (quarter (ceiling (/ month 3.0)))
         (quarter-heading (format "* %d Q%d" year quarter)))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" (regexp-quote quarter-heading) "$") nil t)
        (progn
          (org-end-of-subtree t t)
          (unless (bolp) (insert "\n")))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (concat quarter-heading "\n")))))

(defun my/org-capture-weekly-setup ()
  "Prompt for week start, check for duplicates, then navigate to insertion point."
  (setq my/capture-week-start nil)
  (my/set-capture-week-start)
  (let ((week-heading (concat "** " (my/week-id))))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" (regexp-quote week-heading) "$") nil t)
        (user-error "Week entry %s already exists" (my/week-id))
      (my/org-find-weekly-insertion-point))))

(setq org-capture-templates
      `(
        ("w" "Weekly entry" plain
         (file+function "~/Org/daily.org" my/org-capture-weekly-setup)
         (file "~/.emacs.d/templates/weekly-entry.org")
         :immediate-finish t)
        ("p" "Capture to process with link" plain
         (file+function "~/Org/daily.org" my/org-find-daily-entry)
         "\n- %:description ([[%:link][link]]) #process"
         :immediate-finish t)
        ("q" "Capture to process" plain
         (file+function "~/Org/daily.org" my/org-find-daily-entry)
         "\n- %:description #process"
         :immediate-finish t)
        ("l" "Work log item with link" plain
         (file+function "~/Org/daily.org" my/org-find-daily-entry)
         "\n- %:description ([[%:link][link]]) #log"
         :immediate-finish t)
        ("m" "Work log item" plain
         (file+function "~/Org/daily.org" my/org-find-daily-entry)
         "\n- %:description #log"
         :immediate-finish t)        ("b" "Book" entry
         (file+headline "~/Org/Books.org" "Books")
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
           "\n"))))

;;;; Key Bindings

(global-set-key (kbd "C-c c") 'org-capture)

(provide 'my-org-capture)
;;; my-org-capture.el ends here

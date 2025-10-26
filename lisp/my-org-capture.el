;;; my-org-capture.el --- Org capture templates -*- lexical-binding: t -*-

;;; Commentary:
;; Org capture template configurations

;;; Code:

(require 'org)

;;;; Capture Templates

(setq org-capture-templates
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

;;;; Key Bindings

(global-set-key (kbd "C-c c") 'org-capture)

(provide 'my-org-capture)
;;; my-org-capture.el ends here

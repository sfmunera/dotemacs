;;; my-misc.el --- Miscellaneous packages -*- lexical-binding: t -*-

;;; Commentary:
;;Miscellaneous packages

;;; Code:

;;; Misc

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked"))

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package olivetti
  :hook
  ((org-mode . olivetti-mode)
   (markdown-mode . olivetti-mode)
   (eww-mode . olivetti-mode)
   (nov-mode . olivetti-mode))
  :custom
  (olivetti-body-width 125))

(use-package antlr-mode)

;; Add ansi colors to the compilation mode buffers
(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

;; compilation mode windows should scroll as ouput appears
(setq compilation-scroll-output 'first-error)


;;; RSS reader
(use-package elfeed
  :bind
  ("C-c e" . elfeed)
  (:map elfeed-search-mode-map
        ("w" . elfeed-search-yank)
        ("g" . elfeed-update)
        ("G" . elfeed-search-update--force))
  (:map elfeed-show-mode-map
        ("w" . elfeed-show-yank))
  :config
  (setq elfeed-use-curl nil)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory (concat "~/Dropbox/" ".elfeed/"))
  (setq elfeed-enclosure-default-dir "~/Downloads/")
  (setq elfeed-search-filter "@2-weeks-ago +unread")
  (setq elfeed-sort-order 'descending)
  (setq elfeed-search-clipboard-type 'CLIPBOARD)
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-search-title-min-width 30)
  (setq elfeed-search-trailing-width 25)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-show-unique-buffers t)
  (setq elfeed-search-date-format '("%F %R" 16 :left)))

(use-package elfeed-org
  :after elfeed
  :config
  (setq rmh-elfeed-org-files (list (concat org-directory "elfeed.org")))
  (elfeed-org))

;;; Epub reader
(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(provide 'my-misc)
;;; my-misc.el ends here

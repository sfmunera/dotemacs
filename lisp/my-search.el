;;; my-search.el --- Search tools (grep, ripgrep) -*- lexical-binding: t -*-

;;; Commentary:
;;Search tools (grep, ripgrep)

;;; Code:

;;; Grep
;;; wgrep (writable grep)
(use-package wgrep
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit))
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(provide 'my-search)
;;; my-search.el ends here

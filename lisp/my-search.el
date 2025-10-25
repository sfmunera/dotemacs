;;; my-search.el --- Search tools (grep, ripgrep) -*- lexical-binding: t -*-

;;; Commentary:
;;Search tools (grep, ripgrep)

;;; Code:

;;; Grep
;;; wgrep (writable grep)
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

(provide 'my-search)
;;; my-search.el ends here

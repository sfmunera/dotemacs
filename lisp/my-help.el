;;; my-help.el --- Help and documentation tools -*- lexical-binding: t -*-

;;; Commentary:
;; Help and documentation tools

;;; Code:

;;; Improved help tools

;; Helpful visual auto-completion for keywords
(use-package which-key
  :init (which-key-mode 1)
  :config
  (setq which-key-idle-delay 1))

(use-package hydra)

;; Improved helpful pages
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(provide 'my-help)
;;; my-help.el ends here

;;; my-terminal.el --- Terminal and shell configuration -*- lexical-binding: t -*-

;;; Commentary:
;;Terminal and shell configuration

;;; Code:

;;; Terminal/Shell

(use-package vterm
  :commands vterm
  :custom
  (term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  ;; (vterm-shell "zsh")
  (vterm-max-scrollback 10000))

(defun my/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (setq eshell-history-size 10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
  :hook (eshell-first-time-mode . my/configure-eshell)
  ;; :bind
  ;; (:map eshell-mode-map
  ;;       ("C-r" . counsel-esh-history)
  ;;       ("<home>" . eshell-bol))
  :config
  (eshell-git-prompt-use-theme 'robbyrussell)
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim" "less" "tmux" "screen"))))

(provide 'my-terminal)
;;; my-terminal.el ends here

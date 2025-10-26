;;; my-editing.el --- Editing enhancements -*- lexical-binding: t -*-

;;; Commentary:
;; Packages and settings to improve the editing experience

;;; Code:

;;;; Make C-w kill line if no region selected
(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode 1))

;;;; Environment
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Auto-Saving Changed Files
(use-package super-save
  :defer 1
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

;; Quick access to recently edited files
(use-package recentf
  :bind
  (("C-x C-r" . consult-recent-file))
  :init
  (recentf-mode t)
  :custom
  (recentf-max-saved-items 50))

;; Save history in minibuffer
(use-package savehist
  :config
  (setq history-length 200)
  (setq savehist-additional-variables '(register-alist kill-ring))
  :hook (after-init . savehist-mode))

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Delete the selected text upon text insertion
(use-package delsel
  :hook (after-init . delete-selection-mode))

;; Visual undo
(use-package vundo
  :bind ("C-x u" . vundo)
  :straight (vundo :type git :host github :repo "casouri/vundo"))

;; Only use spaces for indentation
(setq-default indent-tabs-mode nil)

;; Smartparens
(use-package smartparens
  :hook (prog-mode . smartparens-mode))

;; Expand region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Cycle through naming conventions (snake_case, camelCase, PascalCase, UPPER_CASE)
(use-package string-inflection
  :bind ("C-M-j" . string-inflection-cycle))

;; Avy for quick navigation
(use-package avy
  :commands (avy-goto-char-timer avy-goto-word-0 avy-goto-line)
  :bind
  ("M-j" . avy-goto-char-timer)
  :config
  (setq avy-timeout-seconds 0.20))

;; Replace Dabbrev with Hippie expand
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Macros
(require 'kmacro)
(defalias 'kmacro-insert-macro 'insert-kbd-macro)
(define-key kmacro-keymap (kbd "I") #'kmacro-insert-macro)

(provide 'my-editing)
;;; my-editing.el ends here

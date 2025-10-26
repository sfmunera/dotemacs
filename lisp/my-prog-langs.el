;;; my-prog-langs.el --- Language-specific configurations -*- lexical-binding: t -*-

;;; Commentary:
;; Language-specific settings for TypeScript, JavaScript, Python, Rust, etc.

;;; Code:

;;;; TypeScript/JavaScript

;; needs to install LSP for the specific languages first
;; npm install -g typescript-language-server
;; npm install typescript-eslint-language-service -D
(use-package typescript-mode
  :mode "\\.ts\\'"
  :custom
  (typescript-indent-level 2))

;; smart auto-format source code files on save
(use-package apheleia
  ;; :config
  ;; (apheleia-global-mode 1)
  :hook ((typescript-ts-mode js-ts-mode typescript-mode js-mode tsx-ts-mode tsx-mode) . apheleia-mode))

;; js/typescript jest tests
(use-package jest-test-mode
  :commands jest-test-mode
  :hook ((typescript-ts-mode js-ts-mode typescript-mode js-mode tsx-ts-mode tsx-mode) . jest-test-mode))

;;;; Python

;; LSP server installation:
;; pip install "python-lsp-server[all]" or pip install pyright
(use-package python
  :custom
  (python-shell-interpreter "python3"))

;;;; Rust

(use-package rust-mode)

;;;; Snippets

(use-package yasnippet)

(use-package yasnippet-snippets)

(provide 'my-prog-langs)
;;; my-prog-langs.el ends here

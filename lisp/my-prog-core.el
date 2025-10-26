;;; my-prog-core.el --- Core programming configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Core programming settings: tree-sitter, flycheck, and basic prog-mode setup

;;; Code:

;;;; Core Programming Setup

;; Navigate through subwords in camel-cased words correctly.
(add-hook 'prog-mode-hook 'subword-mode)

;;;; Flycheck

(use-package flycheck)

(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)

;;;; Tree Sitter

(use-package treesit
  :straight nil
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             ;; Note the version numbers. These are the versions that
             ;; are known to work with Combobulate *and* Emacs.
             '((bash . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.20.5"))
               (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (java . ("https://github.com/tree-sitter/tree-sitter-java" "v0.20.2"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby" "v0.20.1"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((yaml-mode . yaml-ts-mode)
             (bash-mode . bash-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (tsx-mode . tsx-ts-mode)
             (json-mode . json-ts-mode)
             (css-mode . css-ts-mode)
             (html-mode . html-ts-mode)
             (java-mode . java-ts-mode)
             (rust-mode . rust-ts-mode)
             (python-mode . python-ts-mode)
             (ruby-mode . ruby-ts-mode)
             (js2-mode . js-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             (go-mode . go-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars)
  (use-package combobulate
    :straight nil
    :custom
    (combobulate-key-prefix "C-c o")
    :hook ((prog-mode . combobulate-mode))
    :load-path ("~/.emacs.d/combobulate")))

(provide 'my-prog-core)
;;; my-prog-core.el ends here

;;; my-programming.el --- Programming languages and tools -*- lexical-binding: t -*-

;;; Commentary:
;; Programming languages and tools

;;; Code:

;;; Programming

;; Navigate through subwords in camel-cased words correctly.
(add-hook 'prog-mode-hook 'subword-mode)

;;;; Git

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package git-gutter-fringe)
(use-package git-gutter
  :hook ((prog-mode . git-gutter-mode))
  :config
  (setq git-gutter:update-interval 2)
  (require 'git-gutter-fringe)
  (set-face-foreground 'git-gutter-fr:added "LightGreen")
  (fringe-helper-define 'git-gutter-fr:added nil
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    ".........."
    ".........."
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    ".........."
    ".........."
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    "XXXXXXXXXX")

  (set-face-foreground 'git-gutter-fr:modified "LightGoldenrod")
  (fringe-helper-define 'git-gutter-fr:modified nil
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    ".........."
    ".........."
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    ".........."
    ".........."
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    "XXXXXXXXXX")

  (set-face-foreground 'git-gutter-fr:deleted "LightCoral")
  (fringe-helper-define 'git-gutter-fr:deleted nil
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    ".........."
    ".........."
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    ".........."
    ".........."
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    "XXXXXXXXXX"))

;; These characters are used in terminal mode
(setq git-gutter:modified-sign "≡")
(setq git-gutter:added-sign "≡")
(setq git-gutter:deleted-sign "≡")
(set-face-foreground 'git-gutter:added "LightGreen")
(set-face-foreground 'git-gutter:modified "LightGoldenrod")
(set-face-foreground 'git-gutter:deleted "LightCoral")

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;;; Flycheck
(use-package flycheck)

;;;; Expand region

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;;; Tree sitter

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

;;;; LSP

(use-package ripgrep)

(use-package deadgrep
  :commands deadgrep)

(use-package dumb-jump
  :after xref
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-rg-search-args "--color=never --pcre2 -i")
  (dumb-jump-git-grep-search-args "--color=never -iI --cached --ignore-standard --untracked")
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c e n" . flymake-goto-next-error)
              ("C-c e p" . flymake-goto-prev-error)
              ("C-c e r" . eglot-rename)
              ("C-c e f" . eglot-format)
              ("C-c e b" . eglot-format-buffer)
              ("C-c e a" . eglot-code-actions))
  :hook
  ((python-ts-mode . eglot-ensure)
   (python-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)
   (typescript-mode . eglot-ensure)
   (js-ts-mode . eglot-ensure)
   (js-mode . eglot-ensure)
   (tsx-ts-mode . eglot-ensure)
   (tsx-mode . eglot-ensure)
   (ruby-mode . eglot-ensure)
   (java-mode . eglot-ensure)
   (java-ts-mode . eglot-ensure))
  :config
  ;; (cl-defmethod project-root ((project (head eglot-project)))
  ;;   (cdr project))

  ;; (defun my-project-try-tsconfig-json (dir)
  ;;   (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
  ;;     (cons 'eglot-project found)))

  ;; (add-hook 'project-find-functions
  ;;           'my-project-try-tsconfig-json nil nil)
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(typescript-ts-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(js-ts-mode . ("typescript-language-server" "--stdio")))
  ;; gem install solargraph
  (add-to-list 'eglot-server-programs '(ruby-ts-mode . ("solargraph" "--stdio")))

  (defun eglot-generate-bemol-workspace-folders (server)
    "Generate the workspaceFolders value for the workspace.

This is implemented by returning the content of .bemol/ws_root_folders file"
        (let* ((root (project-root (project-current)))
           (ws-root (file-name-parent-directory
                     (file-name-parent-directory root)))
           (bemol-root (file-name-concat ws-root ".bemol/"))
           (bemol-ws-root-folders (file-name-concat bemol-root "ws_root_folders"))
           (ws-root-folders-content)
           (ws-folders-for-eglot))
      (if (not (file-exists-p bemol-ws-root-folders))
          (eglot-workspace-folders server))
      (setq ws-root-folders-content (with-temp-buffer
                                      (insert-file-contents bemol-ws-root-folders)
                                      (split-string (buffer-string) "\n" t)))
      (setq ws-folders-for-eglot (mapcar (lambda (o) (concat "file://" o))
                                         ws-root-folders-content))
      (vconcat ws-folders-for-eglot)))
  (let ((cache (expand-file-name (md5 (project-root (project-current t)))
                                 (locate-user-emacs-file "jdtls-cache"))))
    (add-to-list 'eglot-server-programs `(java-ts-mode "jdtls" "-data" ,cache
                    ;; The following allows jdtls to find definition
                    ;; if the code lives outside the current project.
                    :initializationOptions
                    ,(lambda (server)
                       `(:workspaceFolders ,(eglot-generate-bemol-workspace-folders server)
                         :extendedClientCapabilities
                         (:classFileContentsSupport t
                                                    :classFileContentsSupport t
		        			    :overrideMethodsPromptSupport t
		        			    :hashCodeEqualsPromptSupport t
		        			    :advancedOrganizeImportsSupport t
		        			    :generateToStringPromptSupport t
		        			    :advancedGenerateAccessorsSupport t
		        			    :generateConstructorsPromptSupport t
		        			    :generateDelegateMethodsPromptSupport t
		        			    :advancedExtractRefactoringSupport t
                                                    :moveRefactoringSupport t
		        			    :clientHoverProvider t
		        			    :clientDocumentSymbolProvider t
		        			    :advancedIntroduceParameterRefactoringSupport t
		        			    :actionableRuntimeNotificationSupport t
                                                    :extractInterfaceSupport t
                                                    :advancedUpgradeGradleSupport t))))))

  ;; The following is to allow eglot to execute some code actions in java. It
  ;; appears that jdtls breaks the LSP interface. and the solution below is
  ;; based on https://github.com/joaotavora/eglot/pull/937.
  (cl-defmethod eglot-execute-command
    (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
    "Eclipse JDT breaks spec and replies with edits as arguments."
    (mapc #'eglot--apply-workspace-edit arguments))

  ;; The jdt server sometimes returns jdt:// scheme for jumping to definition
  ;; instead of returning a file. This is not part of LSP and eglot does not
  ;; handle it. The following code enables eglot to handle jdt files.
  ;; See https://github.com/yveszoundi/eglot-java/issues/6 for more info.
  (defun jdt-file-name-handler (operation &rest args)
    "Support Eclipse jdtls `jdt://' uri scheme."
    (let* ((uri (car args))
           (cache-dir "/tmp/.eglot")
           (source-file
            (directory-abbrev-apply
             (expand-file-name
              (file-name-concat
               cache-dir
               (save-match-data
                 (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri))
                 (message "URI:%s" uri)
                 (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))))))
      (unless (file-readable-p source-file)
        (let ((content (jsonrpc-request (eglot-current-server) :java/classFileContents (list :uri uri)))
              (metadata-file (format "%s.%s.metadata"
                                     (file-name-directory source-file)
                                     (file-name-base source-file))))
          (message "content:%s" content)
          (unless (file-directory-p cache-dir) (make-directory cache-dir t))
          (with-temp-file source-file (insert content))
          (with-temp-file metadata-file (insert uri))))
      source-file))

  (add-to-list 'file-name-handler-alist '("\\`jdt://" . jdt-file-name-handler))

  (defun jdthandler--wrap-legacy-eglot--path-to-uri (original-fn &rest args)
  "Hack until eglot is updated.
ARGS is a list with one element, a file path or potentially a URI.
If path is a jar URI, don't parse. If it is not a jar call ORIGINAL-FN."
  (let ((path (file-truename (car args))))
    (if (equal "jdt" (url-type (url-generic-parse-url path)))
        path
      (apply original-fn args))))

  (defun jdthandler--wrap-legacy-eglot--uri-to-path (original-fn &rest args)
    "Hack until eglot is updated.
ARGS is a list with one element, a URI.
If URI is a jar URI, don't parse and let the `jdthandler--file-name-handler'
handle it. If it is not a jar call ORIGINAL-FN."
    (let ((uri (car args)))
      (if (and (stringp uri)
               (string= "jdt" (url-type (url-generic-parse-url uri))))
          uri
        (apply original-fn args))))

  (defun jdthandler-patch-eglot ()
    "Patch old versions of Eglot to work with Jdthandler."
    (interactive) ;; TODO, remove when eglot is updated in melpa
    (unless (and (advice-member-p #'jdthandler--wrap-legacy-eglot--path-to-uri 'eglot--path-to-uri)
                 (advice-member-p #'jdthandler--wrap-legacy-eglot--uri-to-path 'eglot--uri-to-path))
      (advice-add 'eglot--path-to-uri :around #'jdthandler--wrap-legacy-eglot--path-to-uri)
      (advice-add 'eglot--uri-to-path :around #'jdthandler--wrap-legacy-eglot--uri-to-path)
      (message "[jdthandler] Eglot successfully patched.")))

  ;; invoke
  (jdthandler-patch-eglot)
  :custom
  ;; based on eglot documentation, this improves the performance. I need
  ;; to enable it for debugging eglot issues.
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-send-changes-idle-time 0.5)
  (eglot-auto-display-help-buffer nil)
  (eglot-connection-timeout 120)
  (eglot-confirm-server-initiated-edits nil)
  ;;(eglot-events-buffer-size 0)
  (eglot-extend-to-xref t)
  (eglot-autoshutdown t))

;; (use-package eglot-java
;;   :defer t
;;   :hook ((java-mode java-ts-mode) . eglot-java-mode))

(use-package jarchive
  :after eglot
  :config
  (jarchive-mode 1))

;; (use-package lsp-mode
;;   :hook ((lsp-mode . lsp-diagnostics-mode))
;;   :custom
;;   (lsp-keymap-prefix "C-c l")
;;   (lsp-diagnostics-provider :flymake)
;;   (lsp-completion-provider :capf)
;;   (lsp-session-file (expand-file-name ".lsp-session" user-emacs-directory))
;;   ;;  (lsp-log-io nil)
;;   (lsp-idle-delay 0.5)
;;   ;; core
;;   ;; (lsp-enable-xref t)
;;   ;; (lsp-auto-configure nil)
;;   ;; (lsp-eldoc-enable-hover nil)
;;   ;; (lsp-enable-dap-auto-configure nil)
;;   ;; (lsp-enable-file-watchers nil)
;;   ;; (lsp-enable-folding nil)
;;   ;; (lsp-enable-imenu nil)
;;   ;; (lsp-enable-indentation nil)
;;   ;; (lsp-enable-links nil)
;;   ;; (lsp-enable-on-type-formatting nil)
;;   (lsp-enable-suggest-server-download t)
;;   ;; (lsp-enable-symbol-highlighting nil)
;;   ;; (lsp-enable-text-document-color nil)
;;   ;; ;; completion
;;   ;; (lsp-completion-enable t)
;;   ;; (lsp-completion-enable-additional-text-edit nil)
;;   ;; (lsp-enable-snippet nil)
;;   ;; (lsp-completion-show-kind nil)
;;   ;; ;; headerline
;;   ;; (lsp-headerline-breadcrumb-enable nil)
;;   ;; (lsp-headerline-breadcrumb-enable-diagnostics nil)
;;   ;; (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
;;   ;; (lsp-headerline-breadcrumb-icons-enable nil)
;;   ;; ;; modeline
;;   ;; (lsp-modeline-code-actions-enable nil)
;;   ;; (lsp-modeline-diagnostics-enable nil)
;;   ;; (lsp-modeline-workspace-status-enable nil)
;;   ;; (lsp-signature-doc-lines 1)
;;   ;; ;; lens
;;   ;; (lsp-lens-enable nil)
;;   ;; ;; semantic
;;   ;; (lsp-semantic-tokens-enable nil)
;;   :init
;;   (setq lsp-use-plists t))

;; (use-package lsp-java
;;   :demand t
;;   :after lsp-mode
;;   :custom
;;   (lsp-java-java-path "/usr/lib/jvm/java-17-openjdk-amd64/bin/java"))

;; (use-package lsp-java
;;   :no-require
;;   :hook ((java-mode java-ts-mode) . lsp))

;;;; Languages

;; needs to install LSP for the specific languages first
;; npm install -g typescript-language-server
;; npm install typescript-eslint-language-service -D
(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

;; smart auto-format source code files on save
(use-package apheleia
  ;; :config
  ;; (apheleia-global-mode 1)
  :hook ((typescript-ts-mode js-ts-mode typescript-mode js-mode tsx-ts-mode tsx-mode) . apheleia-mode))

;; js/typescript jest tests
(use-package jest-test-mode
  :commands jest-test-mode
  :hook ((typescript-ts-mode js-ts-mode typescript-mode js-mode tsx-ts-mode tsx-mode) . jest-test-mode))

;; LSP server installation:
;; pip install "python-lsp-server[all]" or pip install pyright
(use-package python
  :custom
  (python-shell-interpreter "python3"))

(use-package rust-mode)

(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)

(use-package yasnippet)

(use-package yasnippet-snippets)

(provide 'my-programming)
;;; my-programming.el ends here

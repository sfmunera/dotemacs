;;; my-prog-lsp.el --- LSP and Eglot configuration -*- lexical-binding: t -*-

;;; Commentary:
;; LSP client configuration: eglot, ripgrep, dumb-jump, jarchive

;;; Code:

;;;; Search Tools

(use-package ripgrep)

(use-package deadgrep
  :commands deadgrep)

;;;; Jump to Definition

(use-package dumb-jump
  :after xref
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-rg-search-args "--color=never --pcre2 -i")
  (dumb-jump-git-grep-search-args "--color=never -iI --cached --ignore-standard --untracked")
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;;; Eglot LSP Client

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

;;;; JAR Archive Support

(use-package jarchive
  :after eglot
  :hook (after-init . jarchive-mode))

;;;; Alternative: LSP Mode (Commented Out)

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

(provide 'my-prog-lsp)
;;; my-prog-lsp.el ends here

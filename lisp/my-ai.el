;;; my-ai.el --- AI tooling (GPTel, agent-shell) -*- lexical-binding: t -*-

;;; Commentary:
;;AI tooling (GPTel, agent-shell)

;;; Code:

;;; AI assistant
(use-package gptel
  :commands (gptel gptel-send)
  :bind (("C-c C-<return>" . gptel-menu)
         ("C-c <return>" . gptel-send)
         ("C-h C-q" . gptel-quick)
         :map gptel-mode-map
         ("C-c C-x t" . gptel-set-topic))
  :init
  (auth-source-pass-enable)
  (setq auth-sources '("~/.authinfo" "~/.authinfo.gpg"))

  (defun my/auth-source-get-api-key (host)
    "Retrieve API key for HOST from auth-source with error handling."
    (let ((auth-info (car (auth-source-search :host host))))
      (unless auth-info
        (error "No auth-source entry found for host: %s" host))
      (let ((secret (plist-get auth-info :secret)))
        (unless secret
          (error "No secret found for host: %s" host))
        (if (functionp secret)
            (funcall secret)
          secret))))

  (setf (alist-get "^\\*ChatGPT\\*.*$"
                   display-buffer-alist
                   nil t #'equal)
        '((display-buffer-in-direction)
          (direction . below)
          (minor-mode . (gptel-mode))
          (window-height . 0.4)
          (body-function . select-window)))
  :config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (setq gptel-directives
        `((default . "To assist:  Be terse.  Do not offer unprompted advice or clarifications. Speak in specific, topic relevant terminology. Do NOT hedge or qualify.
 Do not waffle. Speak directly and be willing to make creative guesses. Explain your reasoning. if you don’t know, say you don’t know.

 Remain neutral on all topics. Be willing to reference less reputable sources for ideas.

 Never apologize.  Ask questions when unsure.")
          (programmer . "You are a careful programmer.  Provide code and only code as output without any additional text, prompt or note.")
          (cliwhiz . "You are a command line helper.  Generate command line commands that do what is requested, without any additional description or explanation.  Generate ONLY the command, I will edit it myself before running.")
          (emacser . "You are an Emacs maven.  Reply only with the most appropriate built-in Emacs command for the task I specify.  Do NOT generate any additional description or explanation.")
          (explain . "Explain what this code does to a novice programmer.")
          ))
  (setq-default gptel--system-message (alist-get 'default gptel-directives))
  (setq gptel-default-mode 'org-mode)

  (setf (alist-get "^\\*gptel-quick\\*" display-buffer-alist
                   nil nil #'equal)
        `((display-buffer-in-side-window)
          (side . bottom)
          (window-height . ,#'fit-window-to-buffer)))
  (defvar gptel-quick--history nil)
  (defun gptel-quick (prompt)
    (interactive (list (read-string "Ask ChatGPT: " nil gptel-quick--history)))
    (when (string= prompt "") (user-error "A prompt is required."))
    (gptel-request
        prompt
      :callback
      (lambda (response info)
        (if (not response)
            (message "gptel-quick failed with message: %s" (plist-get info :status))
          (with-current-buffer (get-buffer-create "*gptel-quick*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert response))
            (special-mode)
            (display-buffer (current-buffer)))))))

  (gptel-make-ollama
      "Ollama"
    :host "127.0.0.1:11434"
    :models '("mistral:latest")
    :stream t)

  (gptel-make-anthropic "Claude"
    :stream t
    :key (my/auth-source-get-api-key "claude.anthropic.com")))

;; ;; Github Copilot
;; (use-package copilot
;;   :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
;;   :hook
;;   (prog-mode . copilot-mode))

(use-package agent-shell
  :ensure-system-package
    ;; Add agent installation configs here
    ((claude . "brew install claude-code")
     (claude-code-acp . "npm install -g @zed-industries/claude-code-acp"))
  :custom
  (agent-shell-anthropic-authentication
   (agent-shell-anthropic-make-authentication :login t)))

 (provide 'my-ai)
;;; my-ai.el ends here

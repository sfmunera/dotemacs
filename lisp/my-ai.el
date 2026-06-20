;;; my-ai.el --- AI tooling (GPTel, agent-shell) -*- lexical-binding: t -*-

;;; Commentary:
;;AI tooling (GPTel, agent-shell)

;;; Code:

;;; AI assistant
(use-package gptel
  :commands (gptel gptel-send)
  :hook ((gptel-mode . gptel-highlight-mode))
  :bind (("C-c C-<return>" . gptel-menu)
         ("C-c <return>" . gptel-send)
         ("C-h C-q" . gptel-quick)
         :map gptel-mode-map
         ("C-c C-x t" . gptel-set-topic))
  :config
  (auth-source-pass-enable)
  (setq auth-sources '("~/.authinfo" "~/.authinfo.gpg"))

  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  
  (setq gptel-display-buffer-action '(pop-to-buffer-same-window)
        gptel-expert-commands t
        gptel-track-media t
        gptel-default-mode 'org-mode
        gptel-prompt-prefix-alist nil
        gptel-response-prefix-alist nil
        gptel-highlight-methods '(face))

  (setq gptel-directives
        `((default . "To assist:  Be terse.  Do not offer unprompted advice or clarifications. Speak in specific, topic relevant terminology. Do NOT hedge or qualify.
 Do not waffle. Speak directly and be willing to make creative guesses. Explain your reasoning. if you don't know, say you don't know.

 Remain neutral on all topics. Be willing to reference less reputable sources for ideas.

 Never apologize.  Ask questions when unsure.")
          (programmer . "You are a careful programmer.  Provide code and only code as output without any additional text, prompt or note.")
          (cliwhiz . "You are a command line helper.  Generate command line commands that do what is requested, without any additional description or explanation.  Generate ONLY the command, I will edit it myself before running.")
          (emacser . "You are an Emacs maven.  Reply only with the most appropriate built-in Emacs command for the task I specify.  Do NOT generate any additional description or explanation.")
          (explain . "Explain what this code does to a novice programmer.")))

  (setq gptel--system-message (alist-get 'default gptel-directives))

  (with-eval-after-load 'gptel-org
    (setq-default gptel-org-branching-context t))

  ;; gptel-quick
  (setf (alist-get "^\\*gptel-quick\\*" display-buffer-alist
                   nil nil #'equal)
        `((display-buffer-in-side-window)
          (side . bottom)
          (window-height . ,#'fit-window-to-buffer)))

  (defvar gptel-quick--history nil)

  (defun gptel-quick (prompt)
    (interactive (list (read-string "Ask LLM: " nil gptel-quick--history)))
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

  ;; Backends
  (gptel-make-anthropic "Claude"
    :stream t
    :key #'gptel-api-key-from-auth-source)

  ;; Defaults
  (setq-default gptel-model 'claude-sonnet-4-5-20250929
                gptel-backend (gptel-get-backend "Claude"))

  ;; Presets
  (gptel-make-preset 'default
    :description "DEFAULT: my standard settings"
    :system 'default
    :backend "Claude"
    :model 'claude-sonnet-4-5-20250929
    :tools nil :temperature nil :stream t
    :include-reasoning nil)

  (gptel-make-preset 'think
    :request-params '(:thinking (:type "enabled" :budget_tokens 1024)
                      :max_tokens 4096))

  (gptel-make-preset 'prog
    :description "PROMPT: Claude
 Sonnet, generates only code"
    :backend "Claude"
    :model 'claude-sonnet-4-5-20250929
    :system 'programmer
    :tools nil :stream t :temperature nil :max-tokens nil
    :use-context 'system
    :include-reasoning nil)

  (gptel-make-preset 'cliwhiz
    :description "PROMPT: generates CLI commands"
    :backend "Claude"
    :model 'claude-sonnet-4-5-20250929
    :system 'cliwhiz
    :tools nil :stream t :temperature 0.66
    :max-tokens nil :use-context nil
    :include-reasoning nil)

  (gptel-make-preset 'explain
    :description "PROMPT: explain the prompt text"
    :parents '(think)
    :backend "Claude"
    :model 'claude-sonnet-4-5-20250929
    :system 'explain
    :tools nil :stream t :temperature nil :max-tokens nil
    :use-context 'system
    :include-reasoning nil))

;; ;; Github Copilot
;; (use-package copilot
;;   :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
;;   :hook
;;   (prog-mode . copilot-mode))

(use-package agent-shell
  :ensure-system-package
    ;; Add agent installation configs here
    ((claude . "brew install claude-code")
     (claude-code-acp . "npm install -g @agentclientprotocol/claude-agent-acp"))
  :custom
  (agent-shell-prefer-viewport-interaction t)
  (agent-shell-display-action '(display-buffer-in-previous-window)))

 (provide 'my-ai)
;;; my-ai.el ends here

;;; my-windows.el --- Window management and navigation -*- lexical-binding: t -*-

;;; Commentary:
;;Window management and navigation

;;; Code:

;;; Window management

;;;; Enhanced ibuffer configuration
(use-package ibuffer
  :bind (([remap list-buffers] . ibuffer)  ; C-x C-b
         :map ibuffer-mode-map
         ("C-k" . ibuffer-do-delete)       ; Quick buffer deletion
         ("M-o" . other-window))           ; Quick window switching
  :custom
  (ibuffer-expert t)                       ; Don't ask for confirmation on delete
  (ibuffer-show-empty-filter-groups nil))  ; Hide empty filter groups

;; Apply some settings from https://www.masteringemacs.org/article/demystifying-emacs-window-manager
;; (defun make-display-buffer-matcher-function (major-modes)
;;   (lambda (buffer-name action)
;;     (with-current-buffer buffer-name (apply #'derived-mode-p major-modes))))

;; ;; Treat manual buffer switching the same as programmatic buffer switching
;; (setq switch-to-buffer-obey-display-actions t)

;; (setq switch-to-buffer-in-dedicated-window 'pop)

;; (setq switch-to-buffer-obey-display-actions t)

;; (add-to-list 'display-buffer-alist
;;              '("\\*helpful.*\\*"
;;                (display-buffer-reuse-window display-buffer-pop-up-window)
;;                (inhibit-same-window . t)))

;; (add-to-list 'display-buffer-alist
;;              '("\\*Help\\*"
;;                (display-buffer-reuse-window display-buffer-pop-up-window)
;;                (inhibit-same-window . t)))

;; (add-to-list 'display-buffer-alist
;;              '("\\*vterm\\*" display-buffer-reuse-mode-window
;;                (inhibit-same-window . t)
;;                (mode vterm-mode vterm-copy-mode)))

;; (add-to-list 'display-buffer-alist
;;              '("\\*Python\\*"
;;                (display-buffer-reuse-mode-window
;;                 display-buffer-in-direction)
;;                (direction . bottom)
;;                (window . root)
;;                (window-height . 0.3)
;;                (inhibit-same-window . t)))

;; (add-to-list 'display-buffer-alist
;;              '("\\*e?shell\\*" display-buffer-in-direction
;;                (direction . bottom)
;;                (window . root)
;;                (window-height . 0.3)))

;; (add-to-list 'display-buffer-alist
;;              `(,(rx (| "*xref*"
;;                        "*grep*"
;;                        "*Occur*"))
;;                display-buffer-reuse-window
;;                (inhibit-same-window . nil)))

;; (setq magit-display-buffer-function #'display-buffer)

;; (add-to-list 'display-buffer-alist
;;              `(,(make-display-buffer-matcher-function '(magit-mode))
;;                (display-buffer-reuse-mode-window
;;                 display-buffer-in-direction)
;;                (mode magit-mode)
;;                (window . root)
;;                (window-width . 0.40)
;;                (direction . right)))

;; ;; left, top, right, bottom
;; (setq window-sides-slots '(0 0 1 0))

;; (add-to-list 'display-buffer-alist
;;              `(,(rx (| "*jest-test-compilation*" "*compilation*" "*grep* *info*"))
;;                display-buffer-in-side-window
;;                display-buffer-reuse-window
;;                (side . right)
;;                (slot . 0)
;;                (window-parameters . ((no-delete-other-windows . t)))
;;                (window-width . 0.4)))

;; winner-mode to undo/redo window layouts
(use-package winner
  :hook (after-init . winner-mode))

(use-package windmove
  :hook (after-init . windmove-mode)
  :bind
  (("s-<left>" . windmove-left)
   ("s-<right>" . windmove-right)
   ("s-<up>" . windmove-up)
   ("s-<down>" . windmove-down)))

;; Jump easily between windows
(use-package ace-window
  :bind (("C-x o" . ace-window))
  :hook (after-init . ace-window-display-mode)
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-minibuffer-flag t))

(defun ace-window-one-command ()
  (interactive)
  (let ((win (aw-select " ACE")))
    (when (windowp win)
      (with-selected-window win
        (let* ((command (key-binding
                         (read-key-sequence
                          (format "Run in %s..." (buffer-name)))))
               (this-command command))
          (call-interactively command))))))

(keymap-global-set "C-x O" 'ace-window-one-command)

(defun ace-window-prefix ()
  "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
  (interactive)
  (display-buffer-override-next-command
   (lambda (buffer _)
     (let (window type)
       (setq
        window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
        type 'reuse)
       (cons window type)))
   nil "[ace-window]")
  (message "Use `ace-window' to display next command buffer..."))

(keymap-global-set "C-x 4 o" 'ace-window-prefix)

;; Default buffer placement options
;; Reuse existing windows especially those with the same mode
(setq display-buffer-base-action
      '((display-buffer-reuse-mode-window
         display-buffer-reuse-window
         display-buffer-same-window
         display-buffer-in-previous-window)))

;; tab-bar-mode
(use-package tab-bar
  :hook ((after-init . tab-bar-mode)
         (after-init . tab-bar-history-mode))
  :bind
  ("s-{" . tab-bar-switch-to-prev-tab)
  ("s-}" . tab-bar-switch-to-next-tab)
  ("s-t" . tab-bar-new-tab)
  ("s-w" . tab-bar-close-tab)
  ("s-r" . tab-bar-rename-tab)
  ("M-[" . tab-bar-history-back)
  ("M-]" . tab-bar-history-forward)
  :custom
  (tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (tab-bar-tab-name-truncated-max 24)
  (tab-bar-new-tab-choice 'ibuffer)
  (tab-bar-select-tab-modifiers '(meta hyper))
  (tab-bar-tab-hints t)
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (tab-bar-close-button-show nil)
  (tab-bar-show 1)) ;; show tabs when more than 1 tab only

(use-package activities
  :hook ((after-init . activities-mode)
         (after-init . activities-tabs-mode))
  :custom
  ;; Prevent `edebug' default bindings from interfering.
  (edebug-inhibit-emacs-lisp-mode-bindings t)
  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)))

(keymap-global-set "C-x w f" 'tear-off-window)
(keymap-global-set "C-x w t" 'tab-window-detach)

;; Change the default behaviour of `other-window' to switch to the most recently used window.
;; Note: This doesn't work if the other window is a pop up like when using gptel-quick (is it because the popped up window has technically not been used?).
(defun other-window-mru ()
  "Select the most recently used window on this frame."
  (interactive)
  (when-let ((mru-window
              (get-mru-window
               nil nil 'not-this-one-dummy)))
    (select-window mru-window)))

(defalias 'other-window-alternating
    (let ((direction 1))
      (lambda (&optional arg)
        "Call `other-window', switching directions each time."
        (interactive)
        (if (equal last-command 'other-window-alternating)
            (other-window (* direction (or arg 1)))
          (setq direction (- direction))
          (other-window (* direction (or arg 1)))))))

(keymap-global-set "M-o" 'other-window-alternating)
;; (keymap-global-set "M-o" 'other-window-mru)

(setq other-window-scroll-default #'get-lru-window)

(defun isearch-other-window (regexp-p)
    "Function to isearch-forward in the next window.

With prefix arg REGEXP-P, perform a regular expression search."
    (interactive "P")
    (unless (one-window-p)
      (with-selected-window (other-window-for-scrolling)
        (isearch-forward regexp-p))))

(keymap-global-set "C-M-s" #'isearch-other-window)

(defun my/next-buffer (&optional arg)
  "Switch to the next ARGth buffer.

With a universal prefix arg, run in the next window."
  (interactive "P")
  (if-let (((equal arg '(4)))
           (win (other-window-for-scrolling)))
      (with-selected-window win
        (next-buffer)
        (setq prefix-arg current-prefix-arg))
    (next-buffer arg)))

(defun my/previous-buffer (&optional arg)
  "Switch to the previous ARGth buffer.

With a universal prefix arg, run in the next window."
  (interactive "P")
  (if-let (((equal arg '(4)))
           (win (other-window-for-scrolling)))
      (with-selected-window win
        (previous-buffer)
        (setq prefix-arg current-prefix-arg))
    (previous-buffer arg)))

(define-key global-map (kbd "C-x C-p") #'my/previous-buffer)
(define-key global-map (kbd "C-x C-n") #'my/next-buffer)
(define-key global-map (kbd "C-x n g") #'set-goal-column)

;; switch-to-buffer, but possibly in the next window by using an argument (e.g. C-u)
(defun my/switch-buffer (&optional arg)
  (interactive "P")
  (run-at-time
   0 nil
   (lambda (&optional arg)
     (if-let (((equal arg '(4)))
              (win (other-window-for-scrolling)))
         (with-selected-window win
           (switch-to-buffer
            (read-buffer-to-switch
             (format "Switch to buffer (%S)" win))))
       (call-interactively #'switch-to-buffer)))
   arg))

;; Enable repeat-mode for buffer cycling
(defvar-keymap buffer-cycle-map
  :doc "Keymap for cycling through buffers, intended for `repeat-mode'."
  :repeat t
  "n" 'my/next-buffer
  "p" 'my/previous-buffer
  "b" 'my/switch-buffer)

;; Associate the commands with the repeat map
(dolist (cmd '(my/next-buffer my/previous-buffer my/switch-buffer))
  (put cmd 'repeat-map 'buffer-cycle-map))

;; Make `pop-global-mark' jump across windows instead of only the current window.
(define-advice pop-global-mark (:around (pgm) use-display-buffer)
  "Make `pop-to-buffer' jump buffers via `display-buffer'."
  (cl-letf (((symbol-function 'switch-to-buffer)
             #'pop-to-buffer))
    (funcall pgm)))

;; (use-package shackle
;;   :config
;;   (setq shackle-lighter "")
;;   (setq shackle-select-reused-windows nil)
;;   (setq shackle-default-alignment 'below)
;;   (setq shackle-default-size 0.4)
;;   (setq shackle-rules
;;         '(
;;           (compilation-mode :select t :size 0.5 :popup t :align t)
;;           ("*Async Shell Command*" :select t :popup t :align t)
;;           ("*Detached Shell Command*" :select t :popup t :align t)
;;           ("*Completions*" :select nil :size 0.3 :align t)
;;           ("*Calendar*" :select t :size 0.3  :align t)
;;           ("*org-timeblock*" :select t :other t :align right)
;;           ("^\\*Warnings\\*$" :regexp t :select nil :size 0.3 :popup t :align t)
;;           (messages-buffer-mode :select nil :size 0.3 :popup t :align t)
;;           ("^\\*Compile-Log\\*$" :regexp t :select nil :popup t :align t)
;;           ("[Oo]utput\\*" :regexp t :select nil :popup t :align t)
;;           ("^\\*Backtrace\\*" :regexp t :select nil :popup t :align t)
;;           ("^\\*Apropos" :regexp t :select nil :popup t :align t)
;;           ("^\\*eldoc\\*" :regexp t :select nil :popup t :align t)
          
;;           ;; Occur/grep modes
;;           (occur-mode :select t :other t :align right)
;;           (grep-mode :select t :other t :align right)
;;           (xref--xref-buffer-mode :select t :other t :align right)
;;           (locate-mode :select t :other t :align right)
;;           (flymake-diagnostics-buffer-mode :select t :other t :align right)
;;           (rg-mode :select t :other t :align right)

;;           ;; REPL modes
;;           (eshell-mode :select t :popup t :align t)
;;           (shell-mode :select t :popup t :align t)
;;           (eat-mode :select t :popup t :align t)
;;           (term-mode :select t :popup t :align t)
;;           (vterm-mode :select t :popup t :align t)
;;           (inferior-python-mode :select t :popup t :align t)
;;           (jupyter-repl-mode :select t :popup t :align t)
;;           ("^\\*\\(?:.*?-\\)\\{0,1\\}e*shell[^z-a]*\\(?:\\*\\|<[[:digit:]]+>\\)$" :regexp t :select t :popup t :align t)
;;           ("\\*.*REPL.*\\*" :regexp t :select t :popup t :align t)
;;           ("*Python*" :select t :popup t :align t)
;;           ("^\\*jupyter-repl.*?\\(\\*\\|<[[:digit:]]>\\)$" :regexp t :select t :popup t :align t)
;;           ("\\*Inferior .*\\*$" :regexp t :select t :popup t :align t)
;;           ("*ielm*" :select t :popup t :align t)
;;           ("*edebug*" :select t :popup t :align t)

;;           ;; Help modes
;;           (helpful-mode :select nil :other t :align right)
;;           (help-mode :select nil :other t :align right)
;;           (Info-mode :select nil :other t :align right)
;;           (pydoc-mode :select nil :other t :align right)
;;           (eldoc-mode :select nil :other t :align right)
;;           (Man-mode :select nil :other t :align right)
;;           (Woman-mode :select nil :other t :align right)
;;           ))
;;   (shackle-mode 1))

(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type)
         ("s-k"   . popper-kill-latest-popup))
  :init
  (defvar my/occur-grep-modes-list '(occur-mode
                                     grep-mode
                                     xref--xref-buffer-mode
                                     locate-mode
                                     flymake-diagnostics-buffer-mode
                                     rg-mode)
    "List of major-modes used in occur-type buffers")
  (defvar my/repl-modes-list '(eshell-mode
                               shell-mode
                               eat-mode
                               vterm-mode
                               inferior-python-mode
                               jupyter-repl-mode)
    "List of major-modes used in REPL buffers")
  (defvar my/repl-names-list
    '("^\\*\\(?:.*?-\\)\\{0,1\\}e*shell[^z-a]*\\(?:\\*\\|<[[:digit:]]+>\\)$"
      "\\*.*REPL.*\\*"
      "\\*Python\\*"
      "^\\*jupyter-repl.*?\\(\\*\\|<[[:digit:]]>\\)$"
      "\\*Inferior .*\\*$"
      "\\*ielm\\*"
      "\\*edebug\\*")
    "List of buffer names used in REPL buffers")
  (defvar my/help-modes-list '(helpful-mode
                               help-mode
                               pydoc-mode
                               eldoc-mode
                               TeX-special-mode)
    "List of major-modes used in documentation buffers")

  (defvar my/man-modes-list '(Man-mode woman-mode)
    "List of major-modes used in Man-type buffers")

  (setq popper-reference-buffers
        (append my/help-modes-list
                my/man-modes-list
                my/repl-modes-list
                my/repl-names-list
                my/occur-grep-modes-list
                '(("^\\*Warnings\\*$" . hide)
                  ("^\\*Compile-Log\\*$" . hide)
                messages-buffer-mode
                "[Oo]utput\\*"
                "\\*Async Shell Command\\*"
                ("\\*Detached Shell Command\\*" . hide)
                compilation-mode
                "^\\*Backtrace\\*"
                "^\\*Apropos"
                "^\\*eldoc\\*"
                "^\\*ChatGPT\\*"
                "^\\*gptel-quick\\*"
                "[Mm]agit"
                "\\*Completions\\*")))
  :hook ((after-init . popper-mode)
         (after-init . popper-echo-mode))
  :custom
  (popper-display-control nil))

(provide 'my-windows)
;;; my-windows.el ends here

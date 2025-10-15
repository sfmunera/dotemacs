;;; init.el --- My Emacs configuration. ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Basic configurations

;;;; Performance optimizations

(setq gc-cons-threshold most-positive-fixnum) ; Disable GC during startup

;;;; Emacs directory
(setq user-emacs-directory (expand-file-name "~/.emacs.d"))

;; move customization variable to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;; Disable creating backup and lock files
(setq make-backup-files nil)
(setq backup-inhibited nil)
(setq create-lockfiles nil)

;;;; Native compilation

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-compile-prune-cache t))

;; Set the right directory to store the native comp cache
;;(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;;;; Start emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;;;; Default coding system
(set-default-coding-systems 'utf-8)

;;;; Prevent Emacs from closing by accident
(setq confirm-kill-emacs 'y-or-n-p)

;;;; Disable prompts
(setq use-short-answers t)
(setq confirm-nonexistent-file-or-buffer nil)
;; Don't show the splash screen
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
;; Don't ask when killing a buffer with a live process attached to it
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Mark location at point without activating region
(defun my/push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-'") 'my/push-mark-no-activate)

(defun my/jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-'") 'my/jump-to-mark)

;;;; Package manager
(setq package-enable-at-startup nil)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq use-package-always-ensure nil)
(setq straight-use-package-version 'straight)
(setq straight-use-package-by-default t)

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)
(straight-use-package 'org)

;; FIX: needed to avoid this error: https://github.com/radian-software/straight.el/issues/1146#issuecomment-1949645571
(straight-use-package 'project)

;;;; Organize files

;; keep folders clean
(setq url-history-file (expand-file-name "url/history" user-emacs-directory))

(use-package no-littering)

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
;; auto-save-mode doesn't create the path automatically
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
(setq create-lockfiles nil)

;; Auto-Saving Changed Files
(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

;; quick access to recently edited files
(use-package recentf
  :bind
  (("C-x C-r" . consult-recent-file))
  :init
  (recentf-mode t)
  :custom
  (recentf-max-saved-items 50))

;; save history in minibuffer
(use-package savehist
  :config
  (setq history-length 200)
  (setq savehist-additional-variables '(register-alist kill-ring))
  :hook (after-init . savehist-mode))

;; remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Delete the selected text upon text insertion
(use-package delsel
  :hook (after-init . delete-selection-mode))

;; Make C-g more helpful
(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

;; Visual undo
(use-package vundo
  :bind ("C-x u" . vundo)
  :straight (vundo :type git :host github :repo "casouri/vundo"))

;;;; Basic UI configuration

;; turn off some unneeded UI elements
(tool-bar-mode -1)
;;(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

;; Don't show UI dialogs when prompting
(setq use-dialog-box nil)


;; replace bell sounds by visual bell
(setq visible-bell t)

;; maximize windows by default
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; make line numbers visible only for programming modes
;; TODO: check why sometimes the numbers dissapear
(column-number-mode)
(global-display-line-numbers-mode 0)
(dolist (mode '(prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(setq blink-cursor-mode nil)

;; Only use spaces for indentation
(setq-default indent-tabs-mode nil)

;; Merge C-a with M-m to go to beginning-of-line or back-to-indentation alternatively
(defun back-to-indentation-or-beginning () (interactive)
       (if (= (point) (progn (back-to-indentation) (point)))
           (beginning-of-line)))

(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)

(use-package face-remap
  :bind
  ;; Emacs 29 introduces commands that resize the font across all
  ;; buffers (including the minibuffer), which is what I want, as
  ;; opposed to doing it only in the current buffer.  The keys are the
  ;; same as the defaults.
  (("C-x C-=" . global-text-scale-adjust)
   ("C-x C-+" . global-text-scale-adjust)
   ("C-x C-0" . global-text-scale-adjust)))

;;; Look and feel

(defun my/set-font-faces ()
  (let ((mono-spaced-font "Aporetic Sans Mono")
        (proportionately-spaced-font "Aporetic Serif"))
    (set-face-attribute 'default nil :family mono-spaced-font :height 130)
    (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
    (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0 :weight 'regular)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (setq doom-modeline-icon t)
                (with-selected-frame frame
                  (my/set-font-faces))))
  (my/set-font-faces))

(use-package spacious-padding
  :config
  (setq spacious-padding-subtle-mode-line nil)
  :hook
  (after-init . spacious-padding-mode))

(setq custom-safe-themes t)

(use-package modus-themes
  :bind (("<f7>" . modus-themes-toggle)
         ("C-<f7>" . modus-themes-select))
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-common-palette-overrides nil
        ;;'((builtin red-cooler))
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-disable-other-themes t
        modus-themes-to-toggle '(modus-operandi modus-vivendi)
        modus-themes-headings
        '((agenda-structure . (variable-pitch light 2.2))
          (agenda-date . (variable-pitch regular 1.3))
          (t . (regular 1.15)))
        ;;'((1 1.3) (2 1.2) (3 1.1))
        )
  ;;:init
  ;;(load-theme 'modus-operandi :no-confirm-loading)
  )

;; TODO: fix heading sizes not getting applied to Org headings
(use-package ef-themes
  :bind (("<f5>" . ef-themes-toggle)
         ("C-<f5>" . ef-themes-select))
  :config
  (setq ef-themes-variable-pitch-ui t
        ef-themes-bold-constructs t
        ef-themes-mixed-fonts t
        ef-themes-disable-other-themes t
        ef-themes-to-toggle '(ef-maris-light ef-maris-dark)
        )
  :init
  (setq ef-themes-headings ; read the manual's entry of the doc string
        '((0 . (variable-pitch light 1.7))
          (1 . (variable-pitch light 1.6))
          (2 . (variable-pitch regular 1.5))
          (3 . (variable-pitch regular 1.4))
          (4 . (variable-pitch regular 1.3))
          (5 . (variable-pitch 1.2)) ; absence of weight means `bold'
          (6 . (variable-pitch 1.1))
          (7 . (variable-pitch 1.1))
          (agenda-date . (semilight 1.3))
          (agenda-structure . (variable-pitch light 1.5))
          (t . (variable-pitch 1.1))))
  (load-theme 'ef-maris-light :no-confirm-loading))

(use-package nerd-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15))

;; Use different colors for nested parens
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; sets the text background to the color mentioned. For example: #0000ff, #ff0000
(use-package rainbow-mode
  :defer t
  :hook (emacs-lisp-mode
         web-mode))

(use-package hl-todo
  :hook
  (prog-mode . hl-todo-mode))

(show-paren-mode 1)

;;; Window management

;;;; Remap listing buffers to ibuffer
(global-set-key [remap list-buffers] 'ibuffer); C-x C-b

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
  :config
  (winner-mode 1)
  :bind
  (("M-[" . winner-undo)
   ("M-]" . winner-redo)))

(use-package windmove
  :config
  (windmove-mode 1)
  :bind
  (("s-<left>" . windmove-left)
   ("s-<right>" . windmove-right)
   ("s-<up>" . windmove-up)
   ("s-<down>" . windmove-down)))

;; TODO: Configure ace-window
;; Jump easily between windows
(use-package ace-window
  :bind (("C-x o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-minibuffer-flag t)
  :config
  (ace-window-display-mode 1))

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
  :init
  (tab-bar-mode 1)
  :bind
  ("s-{" . tab-bar-switch-to-prev-tab)
  ("s-}" . tab-bar-switch-to-next-tab)
  ("s-t" . tab-bar-new-tab)
  ("s-w" . tab-bar-close-tab)
  ("s-r" . tab-bar-rename-tab)
  ("s-<" . tab-bar-history-back)
  ("s->" . tab-bar-history-forward)
  :config
  (tab-bar-history-mode 1)
  (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable
        tab-bar-tab-name-truncated-max 24
        tab-bar-new-tab-choice        'ibuffer
        tab-bar-select-tab-modifiers  '(meta hyper)
        tab-bar-tab-hints             t
        tab-bar-format                '(tab-bar-format-tabs
                                        tab-bar-separator)
        tab-bar-close-button-show     nil))

(use-package activities
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)

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

;; TODO: This doesn't seem to work.
(defvar-keymap buffer-cycle-map
  :doc "Keymap for cycling through buffers, intended for `repeat-mode'."
  :repeat t
  "n" 'my/next-buffer
  "p" 'my/previous-buffer
  "b" 'my/switch-buffer)

;; Make `pop-global-mark' jump across windows instead of only the current window.
(define-advice pop-global-mark (:around (pgm) use-display-buffer)
  "Make `pop-to-buffer' jump buffers via `display-buffer'."
  (cl-letf (((symbol-function 'switch-to-buffer)
             #'pop-to-buffer))
    (funcall pgm)))

(use-package shackle
  :config
  (setq shackle-lighter "")
  (setq shackle-select-reused-windows nil)
  (setq shackle-default-alignment 'below)
  (setq shackle-default-size 0.4)
  (setq shackle-rules
        '(
          (compilation-mode :select t :size 0.5 :popup t :align t)
          ("*Async Shell Command*" :select t :popup t :align t)
          ("*Detached Shell Command*" :select t :popup t :align t)
          ("*Completions*" :select nil :size 0.3 :align t)
          ("*Calendar*" :select t :size 0.3  :align t)
          ("*org-timeblock*" :select t :other t :align right)
          ("^\\*Warnings\\*$" :regexp t :select nil :size 0.3 :popup t :align t)
          (messages-buffer-mode :select nil :size 0.3 :popup t :align t)
          ("^\\*Compile-Log\\*$" :regexp t :select nil :popup t :align t)
          ("[Oo]utput\\*" :regexp t :select nil :popup t :align t)
          ("^\\*Backtrace\\*" :regexp t :select nil :popup t :align t)
          ("^\\*Apropos" :regexp t :select nil :popup t :align t)
          ("^\\*eldoc\\*" :regexp t :select nil :popup t :align t)
          
          ;; Occur/grep modes
          (occur-mode :select t :other t :align right)
          (grep-mode :select t :other t :align right)
          (xref--xref-buffer-mode :select t :other t :align right)
          (locate-mode :select t :other t :align right)
          (flymake-diagnostics-buffer-mode :select t :other t :align right)
          (rg-mode :select t :other t :align right)

          ;; REPL modes
          (eshell-mode :select t :popup t :align t)
          (shell-mode :select t :popup t :align t)
          (eat-mode :select t :popup t :align t)
          (term-mode :select t :popup t :align t)
          (vterm-mode :select t :popup t :align t)
          (inferior-python-mode :select t :popup t :align t)
          (jupyter-repl-mode :select t :popup t :align t)
          ("^\\*\\(?:.*?-\\)\\{0,1\\}e*shell[^z-a]*\\(?:\\*\\|<[[:digit:]]+>\\)$" :regexp t :select t :popup t :align t)
          ("\\*.*REPL.*\\*" :regexp t :select t :popup t :align t)
          ("*Python*" :select t :popup t :align t)
          ("^\\*jupyter-repl.*?\\(\\*\\|<[[:digit:]]>\\)$" :regexp t :select t :popup t :align t)
          ("\\*Inferior .*\\*$" :regexp t :select t :popup t :align t)
          ("*ielm*" :select t :popup t :align t)
          ("*edebug*" :select t :popup t :align t)

          ;; Help modes
          (helpful-mode :select nil :other t :align right)
          (help-mode :select nil :other t :align right)
          (Info-mode :select nil :other t :align right)
          (pydoc-mode :select nil :other t :align right)
          (eldoc-mode :select nil :other t :align right)
          (Man-mode :select nil :other t :align right)
          (Woman-mode :select nil :other t :align right)
          ))
  (shackle-mode 1))

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
  (popper-mode +1)
  (popper-echo-mode +1)
  :config
  (setq popper-display-control nil))

;;; Completions

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)

         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("C-s"   . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; TODO: include these
  ;; (prot-emacs-keybind global-map
  ;;   "M-g M-g" #'consult-goto-line
  ;;   "M-K" #'consult-keep-lines ; M-S-k is similar to M-S-5 (M-%)
  ;;   "M-F" #'consult-focus-lines ; same principle
  ;;   "M-s M-b" #'consult-buffer
  ;;   "M-s M-f" #'consult-find
  ;;   "M-s M-g" #'consult-grep
  ;;   "M-s M-h" #'consult-history
  ;;   "M-s M-i" #'consult-imenu
  ;;   "M-s M-l" #'consult-line
  ;;   "M-s M-m" #'consult-mark
  ;;   "M-s M-y" #'consult-yank-pop
  ;;   "M-s M-s" #'consult-outline)

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode))

;; TODO: check Vertico multiform
(use-package vertico
  :init
  ;; Different scroll margin
  (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  :hook (after-init . vertico-mode))

;; This works with `file-name-shadow-mode' enabled.  When you are in
;; a sub-directory and use, say, `find-file' to go to your home '~/'
;; or root '/' directory, Vertico will clear the old path to keep
;; only your current input.
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(use-package orderless
  :config
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil

        completion-category-overrides
        ;; NOTE 2021-10-25: I am adding `basic' because it works better as a
        ;; default for some contexts.  Read:
        ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50387>.
        ;;
        ;; `partial-completion' is a killer app for files, because it
        ;; can expand ~/.l/s/fo to ~/.local/share/fonts.
        ;;
        ;; If `basic' cannot match my current input, Emacs tries the
        ;; next completion style in the given order.  In other words,
        ;; `orderless' kicks in as soon as I input a space or one of its
        ;; style dispatcher characters.
        '((file (styles . (basic partial-completion orderless)))
          (consult-location (styles . (basic substring initials orderless))))))

;;; Icons
(use-package nerd-icons-completion
  :after (vertico marginalia)
  :config
  (nerd-icons-completion-marginalia-setup)
  (nerd-icons-completion-mode 1)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :hook (after-init . marginalia-mode))

;; TODO: configure embark
(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
                                        ;(add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package corfu
  :straight (corfu :type git :files (:defaults "extensions/*.el"))
  :hook (after-init . global-corfu-mode)
  ;; Optional customizations
  :custom
  (corfu-cycle t)                       ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                        ;; Enable auto completion, check corfu-auto-{prefix,delay}
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 3)
  (corfu-separator ?\s)                 ;; Orderless field separator
  (corfu-quit-at-boundary 'separator)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)               ;; Never quit, even if there is no match
  (corfu-preview-current nil)           ;; Disable current candidate preview
  (corfu-preselect 'first)              ;; Preselect the first candidate
  (corfu-on-exact-match 'insert)        ;; Configure handling of exact matches
  (corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-min-width 20)
  :config
  (setq tab-always-indent 'complete)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode)))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'
  :bind
  (:map corfu-map
        ("C-n" . #'corfu-next)
        ("C-p" . #'corfu-previous)
        ("<escape>" . #'corfu-quit)
        ("<return>" . #'corfu-insert)
        ("<tab>" . #'corfu-complete)
        ("C-<tab>" . corfu-insert-separator)))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


;;; Improved search tools

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

;; TODO: configure avy
(use-package avy
  :commands (avy-goto-char-timer avy-goto-word-0 avy-goto-line)
  :bind
  ("M-j" . avy-goto-char-timer)
  :config
  (setq avy-timeout-seconds 0.20))


;;; Improved help tools

;; Helpful visual auto-completion for keywords
(use-package which-key
  :init (which-key-mode 1)
  :diminish
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

;;; Org mode

;; Org mode configuration
(defun my/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . my/org-mode-setup)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  ;; Custom org mode settings
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis " ▾"
   org-startup-folded 'content

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────"

   ;; Code blocks config
   org-src-fontify-natively t
   org-src-tab-acts-natively t
   org-edit-src-content-indentation 2

   ;; Org files
   org-directory "~/Org/"
   org-default-notes-file "daily.org"
   org-agenda-files (list "~/Org/")
   org-startup-with-inline-images t
   ;; Tags, TODO keywords
   org-log-done 'time
   org-log-into-drawer t
   org-use-fast-todo-selection t
   org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "WAITING(w)" "FOLLOW-UP(f)" "|" "DONE(d)" "CANCELLED(c)"))

   ;; Capture configurations
   org-capture-templates
   `(("b" "Book" entry
      (file+headline "Books.org" "Books")
      ,(mapconcat
        #'identity
        '("*** TO-READ %^{Title}"
          "    :PROPERTIES:"
          "    :ADDED: %U"
          "    :AUTHOR: %^{Author}"
          "    :CATEGORY: %^{Category|Technical|Non-Technical}"
          "    :RATING:"
          "    :DAYS_TO_READ:"
          "    :END:")
        "\n"))
     ("w" "Work")
     ("ww" "Weekly Plan" plain
      (file+olp+datetree "plans.org" "Weekly Plan")
      ,(mapconcat
        #'identity
        '("%?"
          ":PROPERTIES:"
          ":CAPTURED: %U"
          ":END:")
        "\n")
      :empty-lines 1
      :prepend t
      :tree-type week)
     ("wd" "Daily Entry" entry (file "daily.org") "* %<%Y-%m-%d %A>\n%?")
     ("p" "Personal")
     ("pi" "Inbox Capture" entry (file+headline "Inbox.org" "Tasks")
      "* TODO %?\n %U\n" :empty-lines 1 :kill-buffer t)
     ("pj" "Journal" entry (file+olp+datetree "Journal.org")
      "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
      :clock-in :clock-resume :empty-lines 1)))

  ;; set C-a and C-e explicitly because back-to-indentation-or-beginning conflicts with org-special-ctrl-a/e
  (define-key org-mode-map "\C-a" 'org-beginning-of-line)
  (define-key org-mode-map "\C-e" 'org-end-of-line)

  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 10)) (nil . (:maxlevel . 10))))
  (setq org-refile-use-outline-path 'file)
  ;; makes org-refile outline working with completion framework
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way.
  ;; Eveything else will be variable-pitch
  (set-face-attribute 'org-block nil :foreground 'unspecified :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
                                        ;(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-formula nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun my/org-days-between (start-date end-date)
  "Calculate days between two org timestamps"
  (when (and start-date end-date)
    (let ((start (org-time-string-to-time start-date))
          (end (org-time-string-to-time end-date)))
      (floor (/ (float-time (time-subtract end start))
                86400)))))

(defun my/org-get-year (timestamp)
  "Extract year from org timestamp"
  (when timestamp
    (format-time-string "%Y" 
                        (org-time-string-to-time timestamp))))

(defun my/calculate-reading-days ()
  "Calculate and store the reading duration when a book is marked as DONE"
  (let* ((start-date (org-entry-get nil "STARTED"))
         (end-date (org-entry-get nil "FINISHED"))
         (days (my/org-days-between start-date end-date)))
    (when days
      (org-entry-put nil "DAYS_TO_READ" (+ 1 (number-to-string days))))))

(defun my/org-book-state-change ()
  "Handle book state changes"
  (cond
   ((string= org-state "READING")
    (unless (org-entry-get nil "STARTED")  ; Only if STARTED not already set
      (let ((today (format-time-string "[%Y-%m-%d %a]")))
        (org-entry-put nil "STARTED" today)
        ;; Set YEAR_READ to current year when starting
        (org-entry-put nil "YEAR_READ" (my/org-get-year today)))))
   ((string= org-state "READ")
    (let ((today (format-time-string "[%Y-%m-%d %a]")))
      (org-entry-put nil "FINISHED" today)
      ;; Update YEAR_READ to completion year
      (org-entry-put nil "YEAR_READ" (my/org-get-year today))
      (my/calculate-reading-days)
      (org-set-property "RATING" (read-string "Rating (1-5): "))))))

;; Add the hook
(add-hook 'org-after-todo-state-change-hook #'my/org-book-state-change)

(setq org-agenda-custom-commands
      '(("r" "Reading List Overview"
           ((tags "CATEGORY=\"Technical\"|CATEGORY=\"Non-Technical\""
                     ((org-agenda-files '("Books.org"))
                      (org-agenda-prefix-format "  %-12c: ")
                      (org-super-agenda-groups
                       '((:name "Currently Reading"
                          :todo "READING"
                          :order 1)
                         (:name "Technical Books To Read"
                          :and (:todo "TO-READ"
                                :category "Technical")
                          :order 2)
                         (:name "Non-Technical Books To Read"
                          :and (:todo "TO-READ"
                                :category "Non-Technical")
                          :order 3)
                         (:name "Recently Completed Technical Books"
                          :and (:todo "READ"
                                :category "Technical"
                                )
                          :order 4)
                         (:name "Recently Completed Non-Technical Books"
                          :and (:todo "READ"
                                :category "Non-Technical"
                                )
                          :order 5)
                         (:discard (:anything t))))))))
        ("h" . "House Manual Views")
        ("hd" "🏠 House Dashboard"
         ((tags-todo "urgent"
                     ((org-agenda-overriding-header "🚨 URGENT TASKS")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-prefix-format "  %-12c: ")
                      (org-agenda-remove-tags t)
                      (org-agenda-sorting-strategy '(priority-down))
                      ))
          (tags-todo "repair"
                     ((org-agenda-overriding-header "🔧 REPAIRS NEEDED")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-prefix-format "  %-12c: ")
                      (org-agenda-tag-filter-preset '("+repair"))
                      (org-agenda-sorting-strategy '(priority-down))))
          (tags-todo "improvement"
                     ((org-agenda-overriding-header "⬆️  IMPROVEMENTS & UPGRADES")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-prefix-format "  %-12c: ")
                      (org-agenda-tag-filter-preset '("+improvement"))
                      (org-agenda-sorting-strategy '(priority-down))))
          (agenda ""
                  ((org-agenda-overriding-header "📅 THIS WEEK'S MAINTENANCE")
                   (org-agenda-span 7)
                   (org-agenda-start-on-weekday nil)
                   (org-agenda-start-day "today")
                   (org-agenda-tag-filter-preset '("+maintenance"))
                   (org-agenda-prefix-format "  %t: %-12c ")
                   (org-agenda-todo-keyword-format ""))))
         ((org-agenda-files '("Notes/Personal/House.org"))
          (org-agenda-compact-blocks t)
          (org-agenda-block-separator ?─)))
        
        ;; Weekly Planning View
        ("hw" "📅 Weekly Maintenance Plan"
         ((agenda ""
                  ((org-agenda-overriding-header "🗓️  NEXT 7 DAYS")
                   (org-agenda-span 7)
                   (org-agenda-start-on-weekday nil)
                   (org-agenda-start-day "today")
                   (org-agenda-tag-filter-preset '("+maintenance"))
                   (org-agenda-prefix-format "  %t: %-15c %s")
                   (org-agenda-todo-keyword-format "")
                   (org-agenda-scheduled-leaders '("📋 " "📋 "))
                   (org-agenda-time-grid nil))))
         ((org-agenda-files '("Notes/Personal/House.org"))
          (org-agenda-compact-blocks t)))
        
        ;; Monthly Planning View
        ("hm" "📆 Monthly Maintenance Plan"
         ((agenda ""
                  ((org-agenda-overriding-header "🗓️  NEXT 30 DAYS")
                   (org-agenda-span 30)
                   (org-agenda-start-on-weekday nil)
                   (org-agenda-start-day "today")
                   (org-agenda-tag-filter-preset '("+maintenance"))
                   (org-agenda-prefix-format "  %t: %-15c %s")
                   (org-agenda-todo-keyword-format "")
                   (org-agenda-scheduled-leaders '("📋 " "📋 "))
                   (org-agenda-time-grid nil))))
         ((org-agenda-files '("Notes/Personal/House.org"))
          (org-agenda-compact-blocks t)))
        
        ;; Quarterly Planning View
        ("hq" "📊 Quarterly Maintenance Plan"
         ((agenda ""
                  ((org-agenda-overriding-header "🗓️  NEXT 90 DAYS")
                   (org-agenda-span 90)
                   (org-agenda-start-on-weekday nil)
                   (org-agenda-start-day "today")
                   (org-agenda-tag-filter-preset '("+maintenance"))
                   (org-agenda-prefix-format "  %t: %-15c %s")
                   (org-agenda-todo-keyword-format "")
                   (org-agenda-scheduled-leaders '("📋 " "📋 "))
                   (org-agenda-time-grid nil)
                   (org-agenda-show-all-dates nil))))
         ((org-agenda-files '("Notes/Personal/House.org"))
          (org-agenda-compact-blocks t)))
        
        ;; Seasonal View
        ("hs" "🌅 Seasonal Maintenance"
         ((tags-todo "seasonal"
                     ((org-agenda-overriding-header "🌱 SEASONAL TASKS")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-prefix-format "  %-15c: %s")
                      (org-agenda-sorting-strategy '(priority-down)))))
         ((org-agenda-files '("Notes/Personal/House.org"))
          (org-agenda-compact-blocks t)))
        
        ;; Cost Planning View
        ("hc" "💰 Cost Planning"
         ((tags-todo "+TODO=\"TODO\"+COST>0"
                     ((org-agenda-overriding-header "💸 TASKS WITH COSTS")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-prefix-format "  $%-4(org-entry-get nil \"COST\"): %-15c %s")
                      (org-agenda-sorting-strategy '(user-defined-down))))
          
          (tags-todo "urgent+COST>0"
                     ((org-agenda-overriding-header "🚨 URGENT TASKS WITH COSTS")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-prefix-format "  $%-4(org-entry-get nil \"COST\"): %-15c %s"))))
         ((org-agenda-files '("Notes/Personal/House.org"))
          (org-agenda-compact-blocks t)))
        
        ;; All Repairs & Improvements
        ("hr" "🔧 All Repairs & Improvements"
         ((tags-todo "repair+TODO=\"TODO\""
                     ((org-agenda-overriding-header "🔧 ALL REPAIRS")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-prefix-format "  %-15c: %s")
                      (org-agenda-sorting-strategy '(priority-down))))
          
          (tags-todo "improvement+TODO=\"TODO\""
                     ((org-agenda-overriding-header "⬆️  ALL IMPROVEMENTS")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-prefix-format "  %-15c: %s")
                      (org-agenda-sorting-strategy '(priority-down)))))
         ((org-agenda-files '("Notes/Personal/House.org"))
          (org-agenda-compact-blocks t)))

        ("w" "Work Projects and Tasks Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "🗓️ Today"
                                :time-grid t
                                :date today
                                :todo "TODAY"
                                :scheduled today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "\n\n Work Tasks Overview \n━━━━━━━━━━━━━━━━━━━━━━━━━")
                       (org-super-agenda-groups
                        '((:name "⭐ Important Tasks"
                                 :priority "A"
                                 :order 1)
                          (:name "🔥 Active Tasks"
                                 :todo "IN-PROGRESS"
                                 :order 2)
                          (:name "➡️ Ready Tasks"
                                 :todo "NEXT"
                                 :order 3)
                          (:name "Follow Up"
                                 :todo ("FOLLOW-UP")
                                 :order 4)
                          (:name "Tasks waiting for something"
                                 :todo "WAITING"
                                 :order 5)
                          (:name "Nice to Have Tasks"
                                 :priority "B"
                                 :order 6)
                          (:name "📁 Task Backlog"
                                 :todo "TODO"
                                 :order 7)
                          )))))
         ((org-agenda-files (list "~/Org/"))
          (org-agenda-compact-blocks t)))

        ("p" "Personal Projects and Tasks Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "🗓️ Today"
                                :time-grid t
                                :date today
                                :todo "TODAY"
                                :scheduled today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "\n\n✨ PROJECTS ✨\n━━━━━━━━━━━━━━━━━━━━━━━━━")
                       (org-super-agenda-groups
                        '((:discard (:not (:tag "project")))
                          (:name "📦 Active Projects"
                           :todo "ACTIVE"
                           :order 1)
                          (:name "📅 Project Backlog"
                           :todo "BACKLOG"
                           :order 2)
                          (:name "🔥 Active Tasks"
                                 :todo "IN-PROGRESS"
                                 :order 3)
                          (:name "➡️ Next Tasks"
                                 :todo "NEXT"
                                 :order 4)
                          (:name "📋 Task Backlog"
                                 :todo "TODO"
                                 :order 5)
                          ))))
          (alltodo "" ((org-agenda-overriding-header "\n\n✨ GENERAL TASKS ✨\n━━━━━━━━━━━━━━━━━━━━━━━━━")
                       (org-super-agenda-groups
                        '((:discard (:tag "project"))
                          (:name "⭐ Important Tasks"
                                 :priority "A"
                                 :order 1)
                          (:name "🔥 Active Tasks"
                                 :todo "IN-PROGRESS"
                                 :order 2)
                          (:name "➡️ Next Tasks"
                                 :todo "NEXT"
                                 :order 3)
                          (:name "📁 Backlog"
                                 :todo "TODO"
                                 :order 4)
                          (:name "➕ Other Tasks"
                           :auto-category t
                           :order 5))))))
         ((org-agenda-files '("Projects.org" "phone/Inbox.org"))
          (org-agenda-compact-blocks t)))))

(use-package org-modern
  :after org
  :custom-face
  (org-modern-label
   ((t :height 0.8 :width condensed :weight regular
       :underline nil :inherit fixed-pitch)))
  :config
  (global-org-modern-mode))

(require 'org-indent)

(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))



(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-timeblock
  :straight (org-timeblock :type git
              :host github
              :repo "ichernyshovvv/org-timeblock")
  :after org
  :config
  (setq org-timeblock-inbox-file (expand-file-name "daily.org" org-directory)
        org-timeblock-show-outline-path t
        org-timeblock-span 1
        org-timeblock-scale-options '(8 . 18)))

(use-package jupyter
  :straight t
  :after (org))

;; Org babel
;; TODO: Organize into org-mode config
(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)))
;;(org-babel-jupyter-override-src-block "python")


(require 'ob-python)

(setq org-confirm-babel-evaluate nil)

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; shortcut for source blocks: e.g. <el TAB autocompletes the source block
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("rs" . "src rust"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

;; Update table of contents on save
(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

;; My agenda should show me at a glance everything I need to know for the day
;; - Top priority tasks: anything that is marked as "drop everything until this is done"
;; - Next items: what is next to be able to make progress on projects
;; - Waiting: things that I need to follow up on or I'm waiting on
;; - Figure out: Tasks that need to be clarified
;; - Admin/short tasks: tasks that are low effort and can be batched
;; - Learn: Things to learn

;; TODO: Finish setting up my agenda
(use-package org-super-agenda
  :config
  (org-super-agenda-mode))
(setq org-super-agenda-groups
      '((:name "Top Priority"
               :priority "A"
               :order 1)
        (:name "Started"
               :todo ("STARTED")
               :order 2)
        (:name "To Clarify" ;; When not sure what to do yet, needs clarification before READY
               :todo ("CLARIFY")
               :order 3)
        (:name "To Discuss" ;; To discuss during a meeting
               :todo ("TO-DISCUSS")
               :order 4)
        (:name "Follow Up" ;; Follow up on something that doesn't depend on me
               :todo ("FOLLOW-UP")
               :order 5)
        (:name "Waiting" ;; Waiting to hear back from someone
               :todo ("WAITING")
               :order 6)
        (:name "Ready" ;; Ready to start working on
               :todo ("READY")
               :order 7)
        (:name "Scheduled" ;;
               :todo ("SCHEDULED")
               :order 8)
        (:name "On Hold" ;; Temporarily paused or holding on something external
               :todo ("ON-HOLD")
               :order 9)
        (:name "Backburner" ;; Important but not planning to work on yet
               :todo ("BACKBURNER")
               :order 10)
        ))

;;; TRAMP

;; Set default connection mode to SSH
(setq tramp-default-method "ssh")


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
  :diminish
  :hook ((text-mode . git-gutter-mode)
         (prog-mode . git-gutter-mode))
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

;;; Terminal/Shell

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  ;; (setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000))

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


;;; Dired
;; Require to mark by extension
(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :hook
  ((dired-mode . hl-line-mode)
   (dired-mode . dired-hide-details-mode))
  :bind
  (("C-x C-j" . dired-jump)
   (:map dired-mode-map
         ("C-+" . dired-create-empty-file)))
  :config
  ;; Keep only one dired buffer
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
  (setq dired-make-directory-clickable t) ; Emacs 29.1
  (setq dired-free-space nil) ; Emacs 29.1
  (setq dired-mouse-drag-files t) ; Emacs 29.1
  (setq dired-create-destination-dirs 'ask) ; Emacs 27
  (setq dired-create-destination-dirs-on-trailing-dirsep t) ; Emacs 29
  (setq dired-guess-shell-alist-user '(("\\.png" "feh")
                                       ("\\.mkv" "mpv")))
  ;;  (setq dired-listing-switches
  ;;        "-AGFhlv --group-directories-first --time-style=long-iso"))
  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls
        (setq dired-use-ls-dired t
              insert-directory-program gls
              dired-listing-switches "-aBhl  --group-directories-first"))))
  )

(use-package dired-aux
  :straight nil
  :after dired
  :bind
  ( :map dired-mode-map
    ("C-<return>" . dired-do-open)) ; Emacs 30
  :config
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask) ; Emacs 27
  (setq dired-do-revert-buffer t) ; Emacs 28
  (setq dired-create-destination-dirs-on-trailing-dirsep t)) ; Emacs 29

(use-package dired-subtree
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package dired-hide-dotfiles
  :after dired
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind
  (:map dired-mode-map
        ("." . dired-hide-dotfiles-mode)))

(use-package dired-x
  :straight nil
  :after dired
  :config
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t))

(use-package wdired
  :straight nil
  :commands (wdired-change-to-wdired-mode))

(use-package image-dired
  :straight nil
  :commands (image-dired)
  :bind
  ( :map image-dired-thumbnail-mode-map
    ("<return>" . image-dired-thumbnail-display-external))
  :config
  (setq image-dired-thumbnail-storage 'standard)
  (setq image-dired-external-viewer "open")
  (setq image-dired-thumb-size 80)
  (setq image-dired-thumb-margin 2)
  (setq image-dired-thumb-relief 0)
  (setq image-dired-thumbs-per-row 4))

(use-package ready-player
  :straight t
  :mode
  ("\\.\\(mp3\\|m4a\\|mp4\\|mkv\\|webm\\)\\'" . ready-player-major-mode)
  :config
  (setq ready-player-autoplay nil)
  (setq ready-player-repeat nil))

;;; dired-like mode for the trash (trashed.el)
(use-package trashed
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package nerd-icons-dired
  :config
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

;; Customizations to dired
(defvar prot-dired--limit-hist '()
  "Minibuffer history for `prot-dired-limit-regexp'.")

;;;###autoload
(defun prot-dired-limit-regexp (regexp omit)
  "Limit Dired to keep files matching REGEXP.

With optional OMIT argument as a prefix (\\[universal-argument]),
exclude files matching REGEXP.

Restore the buffer with \\<dired-mode-map>`\\[revert-buffer]'."
  (interactive
   (list
    (read-regexp
     (concat "Files "
             (when current-prefix-arg
               (propertize "NOT " 'face 'warning))
             "matching PATTERN: ")
     nil 'prot-dired--limit-hist)
    current-prefix-arg))
  (dired-mark-files-regexp regexp)
  (unless omit (dired-toggle-marks))
  (dired-do-kill-lines)
  (add-to-history 'prot-dired--limit-hist regexp))

(defvar prot-dired--find-grep-hist '()
  "Minibuffer history for `prot-dired-grep-marked-files'.")

;; Also see `prot-search-grep' from prot-search.el.
;;;###autoload
(defun prot-dired-grep-marked-files (regexp &optional arg)
  "Run `find' with `grep' for REGEXP on marked files.
When no files are marked or when just a single one is marked,
search the entire directory instead.

With optional prefix ARG target a single marked file.

We assume that there is no point in marking a single file and
running find+grep on its contents.  Visit it and call `occur' or
run grep directly on it without the whole find part."
  (interactive
   (list
    (read-string "grep for PATTERN (marked files OR current directory): " nil 'prot-dired--find-grep-hist)
    current-prefix-arg)
   dired-mode)
  (when-let* ((marks (dired-get-marked-files 'no-dir))
              (files (mapconcat #'identity marks " "))
              (args (if (or arg (length> marks 1))
                        (format "grep -nH --color=auto %s %s" (shell-quote-argument regexp) files)
                      (concat
                       "find . -not " (shell-quote-argument "(")
                       " -wholename " (shell-quote-argument "*/.git*")
                       " -prune " (shell-quote-argument ")")
                       " -type f"
                       " -exec grep -nHE --color=auto " regexp " "
                       (shell-quote-argument "{}")
                       " " (shell-quote-argument ";") " "))))
    (compilation-start
     args
     'grep-mode
     (lambda (mode) (format "*prot-dired-find-%s for '%s'" mode regexp))
     t)))

;;; Grep
;;; wgrep (writable grep)
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

;;; Misc

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked"))

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package olivetti
  :hook
  ((org-mode . olivetti-mode)
   (markdown-mode . olivetti-mode)
   (eww-mode . olivetti-mode)
   (nov-mode . olivetti-mode))
  :custom
  (olivetti-body-width 125))

(use-package antlr-mode)

;; Add ansi colors to the compilation mode buffers
(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

;; compilation mode windows should scroll as ouput appears
(setq compilation-scroll-output 'first-error)


;;; RSS reader
(use-package elfeed
  :ensure t
  :bind
  ("C-c e" . elfeed)
  (:map elfeed-search-mode-map
        ("w" . elfeed-search-yank)
        ("g" . elfeed-update)
        ("G" . elfeed-search-update--force))
  (:map elfeed-show-mode-map
        ("w" . elfeed-show-yank))
  :config
  (setq elfeed-use-curl nil)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory (concat "~/Dropbox/" ".elfeed/"))
  (setq elfeed-enclosure-default-dir "~/Downloads/")
  (setq elfeed-search-filter "@2-weeks-ago +unread")
  (setq elfeed-sort-order 'descending)
  (setq elfeed-search-clipboard-type 'CLIPBOARD)
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-search-title-min-width 30)
  (setq elfeed-search-trailing-width 25)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-show-unique-buffers t)
  (setq elfeed-search-date-format '("%F %R" 16 :left)))

(use-package elfeed-org
  :after elfeed
  :config
  (setq rmh-elfeed-org-files (list (concat org-directory "elfeed.org")))
  (elfeed-org))

;;; Epub reader
(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

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
    (funcall (plist-get (car (auth-source-search
               'secret host)) :secret)))

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

  ;; TODO: Fix auth for Claude
  (gptel-make-anthropic "Claude"          ;Any name you want
    :stream t                             ;Streaming responses
    :key (my/auth-source-get-api-key "claude.anthropic.com")))

;; ;; Github Copilot
;; (use-package copilot
;;   :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
;;   :hook
;;   (prog-mode . copilot-mode))

;;; Macros

(require 'kmacro)
(defalias 'kmacro-insert-macro 'insert-kbd-macro)
(define-key kmacro-keymap (kbd "I") #'kmacro-insert-macro)

;;; Work-specific config (private)
(let ((work-config-file (expand-file-name "work-config.el" user-emacs-directory)))
  (when (file-exists-p work-config-file)
    (condition-case err
        (load work-config-file)
      (error (message "Failed to load work config: %s" err)))))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)) ; 16MB
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
;;; init.el ends here

;; -*- lexical-binding: t; -*-

;;; Basic configurations

;;;; Performance optimizations

;; Make startup faster by reducing the frequency of garbage collection
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;;; Emacs directory
(setq user-emacs-directory (expand-file-name "~/.emacs.d"))

;;;; Native compilation

;; Silence native compiling warnings as they are pretty noisy
(setq native-comp-async-report-warnings-errors nil)

;; Set the right directory to store the native comp cache
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;;;; Personal information
(setq user-full-name "Sebastian Munera")

;; Leaving this commented out as the emacs daemon is now started by the system
;; (server-start)

;;;; Default coding system
(set-default-coding-systems 'utf-8)


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
(defun sm/push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-`") 'sm/push-mark-no-activate)

(defun sm/jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'sm/jump-to-mark)

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

(setq straight-use-package-by-default t)

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;;;; Organize files

;; keep folders clean
(setq url-history-file (expand-file-name "url/history" user-emacs-directory))

(use-package no-littering)

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
;; auto-save-mode doesn't create the path automatically
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
(setq create-locksfiles nil)

;; Auto-Saving Changed Files
(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

;; move customization variable to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; quick access to recently edited files
(use-package recentf
  :bind
  (("C-x C-r" . consult-recent-file))
  :init
  (recentf-mode t)
  :custom
  (recentf-max-saved-items 50))

;; save history in minibuffer
(setq history-length 25)
(savehist-mode 1)

;; remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Visual undo
;; TODO: fix vundo
(use-package vundo
  :commands (vundo)
  :bind (("C-x u" . vundo))
  :straight (vundo :type git :host github :repo "casouri/vundo"))

;;;; Basic UI configuration

;; turn off some unneeded UI elements
(tool-bar-mode -1)
(scroll-bar-mode -1)
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

;;; Look and feel

(defun sm/set-font-faces ()
  (set-face-attribute 'default nil :family "Iosevka Comfy" :height 150)
  (set-face-attribute 'fixed-pitch nil :family "Iosevka Comfy" :height 150)
  (set-face-attribute 'variable-pitch nil :family "Iosevka Comfy Duo" :height 150 :weight 'regular))

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(setq doom-modeline-icon t)
		(with-selected-frame frame
		  (sm/set-font-faces))))
  (sm/set-font-faces))

(use-package spacious-padding
  :hook
  (text-mode . spacious-padding-mode))

;;;; Subtle mode line style
(defun prot-modeline-set-faces (_theme)
  "Make THEME mode lines subtle."
  (let ((subtle (face-foreground 'shadow)))
    (custom-set-faces
     `(mode-line ((t :background unspecified :box unspecified :overline ,subtle)))
     `(mode-line-active ((t :inherit mode-line :box unspecified)))
     `(mode-line-inactive ((t :background unspecified :foreground ,subtle :box unspecified :overline ,subtle))))))

(defun prot-modeline-unset-faces ()
  "Make window dividers for THEME invisible."
  (custom-set-faces
   `(mode-line (( )))
   `(mode-line-active (( )))
   `(mode-line-inactive (( )))))

(defun prot-modeline--enable-mode ()
  "Enable `prot-modeline-subtle-mode'."
  (prot-modeline-set-faces nil)
  (add-hook 'enable-theme-functions #'prot-modeline-set-faces))

(defun prot-modeline--disable-mode ()
  "Disable `prot-modeline-subtle-mode'."
  (prot-modeline-unset-faces)
  (remove-hook 'enable-theme-functions #'prot-modeline-set-faces))

    ;;;###autoload
(define-minor-mode prot-modeline-subtle-mode
  "Increase the padding/spacing of frames and windows."
  :global t
  (if prot-modeline-subtle-mode
      (prot-modeline--enable-mode)
    (prot-modeline--disable-mode)))

(prot-modeline-subtle-mode)

(use-package modus-themes
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-common-palette-overrides '((builtin red-cooler))
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui nil
        modus-themes-disable-other-themes t
        modus-themes-to-toggle '(modus-operandi modus-vivendi)
        modus-themes-headings '((1 1.3) (2 1.2) (3 1.1)))
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle)
  :init
  (load-theme 'modus-operandi))

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
  :hook (org-mode
         emacs-lisp-mode
         web-mode))



;;; Window management

;; TODO: Add dashboard
;; TODO: Create keyboard macro to recreate all my tab and windows

;; TODO: fix some of these configurations: windmove stops working, previewing buffers screws up window layout for magit while searching for buffer
;;;; Remap listing buffers to ibuffer
(global-set-key [remap list-buffers] 'ibuffer); C-x C-b

;; Apply some settings from https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(defun make-display-buffer-matcher-function (major-modes)
  (lambda (buffer-name action)
    (with-current-buffer buffer-name (apply #'derived-mode-p major-modes))))

;; Treat manual buffer switching the same as programmatic buffer switching
(setq switch-to-buffer-obey-display-actions t)

(setq switch-to-buffer-in-dedicated-window 'pop)

(setq switch-to-buffer-obey-display-actions t)

(add-to-list 'display-buffer-alist
             '("\\*helpful.*\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)))

(add-to-list 'display-buffer-alist
             '("\\*Help\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)))

(add-to-list 'display-buffer-alist
             '("\\*vterm\\*" display-buffer-reuse-mode-window
               (inhibit-same-window . t)
               (mode vterm-mode vterm-copy-mode)))

(add-to-list 'display-buffer-alist
             '("\\*Python\\*"
               (display-buffer-reuse-mode-window
                display-buffer-in-direction)
               (direction . bottom)
               (window . root)
               (window-height . 0.3)
               (inhibit-same-window . t)))

(add-to-list 'display-buffer-alist
             '("\\*e?shell\\*" display-buffer-in-direction
               (direction . bottom)
               (window . root)
               (window-height . 0.3)))

(add-to-list 'display-buffer-alist
             `(,(rx (| "*xref*"
                       "*grep*"
                       "*Occur*"))
               display-buffer-reuse-window
               (inhibit-same-window . nil)))

(setq magit-display-buffer-function #'display-buffer)

(add-to-list 'display-buffer-alist
             `(,(make-display-buffer-matcher-function '(magit-mode))
               (display-buffer-reuse-mode-window
                display-buffer-in-direction)
               (mode magit-mode)
               (window . root)
               (window-width . 0.40)
               (direction . right)))

;; left, top, right, bottom
(setq window-sides-slots '(0 0 1 0))

(add-to-list 'display-buffer-alist
             `(,(rx (| "*compilation*" "*grep* *info*"))
               display-buffer-in-side-window
               display-buffer-reuse-window
               (side . right)
               (slot . 0)
               (window-parameters . ((no-delete-other-windows . t)))
               (window-width . 80)))

;; Frame Scaling / Zooming
;; The keybindings for this are C+M+- and C+M+=.
(use-package default-text-scale
  :defer 1
  :bind
  (("s--" . default-text-scale-decrease)
   ("s-=" . default-text-scale-increase))
  :config
  (default-text-scale-mode))

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

;; Jump easily between windows
(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-minibuffer-flag t)
  :config
  (ace-window-display-mode 1))

;; Default buffer placement options
;; Reuse existing windows especially those with the same mode
(setq display-buffer-base-action
      '((display-buffer-reuse-window
	 display-buffer-reuse-mode-window
	 display-buffer-same-window
	 display-buffer-in-previous-window)))

;; tab-bar-mode
(use-package tab-bar
  :init
  (tab-bar-mode 1)
  :custom
  (tab-bar-show 1)                      ;; hide bar if <= 1 tabs open
  (tab-bar-close-button-show nil)       ;; hide tab close / X button
  (tab-bar-tab-hints t)                 ;; show tab numbers
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  :bind
  ("s-{" . tab-bar-switch-to-prev-tab)
  ("s-}" . tab-bar-switch-to-next-tab)
  ("s-t" . tab-bar-new-tab)
  ("s-w" . tab-bar-close-tab)
  ("s-r" . tab-bar-rename-tab)
  ("s-]" . tab-bar-move-tab)
  ("s-[" . tab-bar-select-tab))


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

(use-package vertico
  :init
  (vertico-mode)
  :bind (("C-s" . consult-line)) ;; TODO: put this in the consult binds

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; This works with `file-name-shadow-mode' enabled.  When you are in
;; a sub-directory and use, say, `find-file' to go to your home '~/'
;; or root '/' directory, Vertico will clear the old path to keep
;; only your current input.
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(setq completion-category-overrides
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
      '((file (styles . (basic partial-completion orderless)))))


(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

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

(setq tab-always-indent 'complete)

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
  ;; Optional customizations
  :custom
  (corfu-cycle t)                       ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                        ;; Enable auto completion, check corfu-auto-{prefix,delay}
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 3)
  (corfu-separator ?\s)                 ;; Orderless field separator
  (corfu-quit-at-boundary 'separator)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)             ;; Never quit, even if there is no match
  (corfu-preview-current 'insert)       ;; Disable current candidate preview
  (corfu-preselect 'prompt)             ;; Preselect the prompt
  (corfu-on-exact-match nil)            ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)               ;; Use scroll margin
  (corfu-popupinfo-delay 0)
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode)))
  :bind
  (:map corfu-map
        ("C-n" . #'corfu-next)
        ("C-p" . #'corfu-previous)
        ("<escape>" . #'corfu-quit)
        ("<return>" . #'corfu-insert)
        ("<return>" . #'corfu-complete)
        ("C-<tab>" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;;; Improved search tools

(use-package smartparens
  :hook (prog-mode . smartparens-mode))


(use-package avy
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line)
  :bind
  ("C-: c" . avy-goto-char)
  ("C-: w" . avy-goto-word-0)
  ("C-: l" . avy-goto-line))


;;; Improved help tools

;; Helpful visual auto-completion for keywords
;; TODO: Check why it stopped working
(use-package which-key
  :init (which-key-mode 1)
  :diminish
  :config
  (setq which-key-idle-delay 0.3))

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
(defun sm/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . sm/org-mode-setup)
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
   org-directory "~/Dropbox/org/"
   org-default-notes-file "Inbox.org"
   org-agenda-files '("Work.org")

   ;; Tags, TODO keywords
   org-log-done 'time
   org-log-into-drawer t
   org-use-fast-todo-selection t
   org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "DOING(i)" "|" "DONE(d)")
     (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))

   ;; Capture configurations
   ;; Work
   ;;  [X] Work inbox: timestamp, what
   ;;  [-] Meeting notes: timestamp, clock, attendees, notes, action items. Refile action items to inbox
   ;;  [X] Daily journal: date. Refile some things to work log.
   ;;  [-] Weekly plan: date, template with goals, sub-headers with days of the week
   ;;  [X] Daily plan: meant to use with agenda side by side. Date, goals.
   ;;  [X] New project: date, project name, project description, sub-headers for tasks
   ;;  [-] Weekly work log: refine the items that are refiled from daily journal. Template: deliveries, who I helped, feedback received, important artifacts, important discussions, how I influenced, important meetings, important decisions, what did I learn.
   org-capture-templates
   `(
     ("w" "Work")
     ("wi" "Work Inbox" entry
      (file+headline "Work.org" "Inbox")
      ,(mapconcat
        #'identity
        '("* TODO %? :inbox:"
          ":PROPERTIES:"
          ":CAPTURED: %U"
          ":END:")
        "\n")
      :empty-lines 1)
     ("wm" "Meeting Notes" entry
      (file+olp+datetree "Work.org" "Meetings")
      ,(mapconcat
        #'identity
        '("* Meeting: \"%?\" :meeting:"
          ":PROPERTIES:"
          ":CAPTURED: %U"
          ":END:")
        "\n")
      :prepend t
      :empty-lines 1
      :tree-type week)
     ("wj" "Daily Journal" plain
      (file+olp+datetree "Work.org" "Daily Journal")
      ,(mapconcat
        #'identity
        '("%?"
          ":PROPERTIES:"
          ":CAPTURED: %U"
          ":END:")
        "\n")
      :empty-lines 1
      :prepend t)
     ("ww" "Weekly Plan" plain
      (file+olp+datetree "Work.org" "Weekly Plan")
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
     ("wd" "Daily Plan" plain
      (file+olp+datetree "Work.org" "Daily Plan")
      ,(mapconcat
        #'identity
        '("%?"
          ":PROPERTIES:"
          ":CAPTURED: %U"
          ":END:")
        "\n")
      :prepend t
      :empty-lines 1)
     ("wp" "New Project" entry
      (file+headline "Work.org" "Projects")
      ,(mapconcat
        #'identity
        '("* TODO %? :project:"
          ":PROPERTIES:"
          ":CAPTURED: %U"
          ":END:")
        "\n")
      :prepend t
      :empty-lines 1)
     ("wl" "Work Log Entry" plain
      (file+olp+datetree "Work.org" "Work Log")
      ,(mapconcat
        #'identity
        '("%?"
          ":PROPERTIES:"
          ":CAPTURED: %U"
          ":END:")
        "\n")
      :empty-lines 1)
     
     ("p" "Personal")
     ("pi" "Inbox Capture" entry (file+headline "Inbox.org" "Tasks")
      "* TODO %?\n %U\n" :empty-lines 1 :kill-buffer t)
     ("pj" "Journal" entry (file+olp+datetree "Journal.org")
      "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
      :clock-in :clock-resume :empty-lines 1)))

  ;; set C-a and C-e explicitly because back-to-indentation-or-beginning conflicts with org-special-ctrl-a/e
  (define-key org-mode-map "\C-a" 'org-beginning-of-line)
  (define-key org-mode-map "\C-e" 'org-end-of-line)

  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 2)) (nil . (:maxlevel . 2))))
  (setq org-refile-use-outline-path 'file)
  ;; makes org-refile outline working with completion framework
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way.
  ;; Eveything else will be variable-pitch
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
                                        ;(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-formula nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org-modern
  :after org
  :bind
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

;; Org babel
;; TODO: Organize into org-mode config
(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   ;;   (python . t)
   (shell . t)))

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


;;; TRAMP

;; Set default connection mode to SSH
(setq tramp-default-method "ssh")


;;; Programming

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


;;;; Project management

(use-package project)

;;;; Flycheck
(use-package flycheck)

;;;; Tree sitter

;; Grammar libraries are installed in ~/.emacs.d/tree-sitter/ and
;; downloaded from
;; https://github.com/emacs-tree-sitter/tree-sitter-langs/releases for
;; all platforms
(dolist (mapping '((yaml-mode . yaml-ts-mode)
                   (bash-mode . bash-ts-mode)
                   (js-mode . js-ts-mode)
                   (js2-mode . js2-ts-mode)
                   (typescript-mode . typescript-ts-mode)
                   (tsx-mode . tsx-ts-mode)
                   (json-mode . json-ts-mode)
                   (css-mode . css-ts-mode)
                   (elisp-mode . elisp-ts-mode)
                   (java-mode . java-ts-mode)
                   (rust-mode . rust-ts-mode)
                   (python-mode . python-ts-mode)))
  (add-to-list 'major-mode-remap-alist mapping))

(setq treesit-font-lock-level 4)

(use-package combobulate
  :preface
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (setq combobulate-key-prefix "C-c o")

  ;; Optional, but recommended.
  ;;
  ;; You can manually enable Combobulate with `M-x
  ;; combobulate-mode'.
  :hook ((yaml-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode)         
         (json-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (python-ts-mode . combobulate-mode))
  ;; Amend this to the directory where you keep Combobulate's source
  ;; code.
  :load-path ("~/.emacs.d/combobulate"))

;;;; LSP

(use-package eglot
  :ensure t
  :bind (:map eglot-mode-map
              ("C-c e n" . flymake-goto-next-error)
              ("C-c e p" . flymake-goto-prev-error)
              ("C-c e r" . eglot-rename)
              ("C-c e f" . eglot-format)
              ("C-c e b" . eglot-format-buffer)
              ("C-c e a" . eglot-code-actions))
  :hook
  ((python-ts-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)
   (js-ts-mode . eglot-ensure)
   (tsx-ts-mode . eglot-ensure))
  :config
  ;; I'm not sure why this is needed, but it throws an error if I remove it
  (cl-defmethod project-root ((project (head eglot-project)))
    (cdr project))

  (defun my-project-try-tsconfig-json (dir)
    (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
      (cons 'eglot-project found)))

  (add-hook 'project-find-functions
            'my-project-try-tsconfig-json nil nil)
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(typescript-ts-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(js-ts-mode . ("typescript-language-server" "--stdio")))
  :custom
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref t)
  (eglot-autoshutdown t)
  (eglot-stay-out-of '(yasnippet)))

(use-package eglot-java
  :ensure t
  :defer t
  :hook ((java-ts-mode . eglot-java-mode)))

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

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

(use-package jest-test-mode 
  :ensure t 
  :commands jest-test-mode
  :hook (typescript-ts-mode js-ts-mode typescript-mode js-mode tsx-ts-mode tsx-mode))

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

(defun sm/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (setq eshell-history-size 10000
	eshell-buffer-maximum-lines 10000
	eshell-hist-ignoredups t
	eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

;; TODO: Fix eshell
(use-package eshell
  :hook (eshell-first-time-mode . sm/configure-eshell)
  :bind
  (:map eshell-mode-map
        ("C-r" . counsel-esh-history)
        ("<home>" . eshell-bol))
  :config
  (eshell-git-prompt-use-theme 'powerline)
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim" "less" "tmux" "screen"))))


;;; Dired
;; Require to mark by extension
(require 'dired-x)
;; Keep only one dired buffer
(use-package dired-single)

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind
  (("C-x C-j" . dired-jump)
   (:map dired-mode-map
	 ("h" . dired-single-up-directory)
	 ("l" . dired-single-buffer)))
  :custom
  ;; ;; In MacOS run this: brew install coreutils
  ;; https://stackoverflow.com/questions/25125200/emacs-error-ls-does-not-support-dired
  ((when (string= system-type "darwin")
     (setq dired-use-ls-dired t
           insert-directory-program "/usr/local/bin/gls"))
   (setq dired-listing-switches "-agho --group-directories-first")))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(setq dired-guess-shell-alist-user '(("\\.png" "feh")
                                     ("\\.mkv" "mpv")))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind
  (:map dired-mode-map
	("H" . dired-hide-dotfiles-mode)))


;;; Misc

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked"))

(use-package olivetti
  :ensure t
  :bind
  ("C-c x o" . olivetti-mode)
  :hook
  ((org-mode . olivetti-mode)
   (markdown-mode . olivetti-mode))
  :custom
  (olivetti-body-width 100))

(use-package antlr-mode)

;;; Work-specific config (private)

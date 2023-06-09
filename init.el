;; TODO: Transform this into a Org config file

;; Make startup faster by reducing the frequency of garbage collection
;; and then use a hook to measure Emacs startup time.

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Default coding system
(set-default-coding-systems 'utf-8)

;; Only use spaces for indentation
(setq-default indent-tabs-mode nil)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(server-start)

;; Don't show the splash screen
(setq inhibit-startup-message t)

;; turn off some unneeded UI elements
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

;; replace bell sounds by visual bell
(setq visible-bell t)

;; maximize windows by default
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; keep folders clean
(setq user-emacs-directory (expand-file-name "~/.cache/emacs")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

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

(defun sm/set-font-faces ()
  (set-face-attribute 'default nil :font "Source Code Pro" :height 130)
  (set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height 125)
  (set-face-attribute 'variable-pitch nil :font "Source Sans Pro" :height 150 :weight 'regular))

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(setq doom-modeline-icon t)
		(with-selected-frame frame
		  (sm/set-font-faces))))
  (sm/set-font-faces))

;; make line numbers visible
(column-number-mode)
;;(global-display-line-numbers-mode t)

;; Disable line number for some modes
(dolist (mode '(text-mode-hook
		prog-mode-hook
		conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq blink-cursor-mode nil)

;; quick access to recently edited files
(recentf-mode 1)
(global-set-key (kbd "C-c f") 'recentf-open-files)

;; save history in minibuffer
(setq history-length 25)
(savehist-mode 1)

;; remember and restore the last cursor location of opened files
(save-place-mode 1)

;; move customization variable to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Don't show UI dialogs when prompting
(setq use-dialog-box nil)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

(use-package undo-tree
 :init
 (global-undo-tree-mode 1))

;; Frame Scaling / Zooming
;; The keybindings for this are C+M+- and C+M+=.

(use-package default-text-scale
  :defer 1
  :config
  (default-text-scale-mode))

;; winner-mode to undo/redo window layouts
(use-package winner
  :config
  (winner-mode))

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

;; TODO: tab-bar-mode


;; Package system and settings
(require 'package)
(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org"          . "https://orgmode.org/elpa/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
			 ("nongnu"       . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Auto package upgrades
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; Solarized theme
;; (use-package solarized-theme
;;   :init (load-theme 'solarized-dark t))
;; Gruvbox theme
;; (use-package gruvbox-theme
;;   :init (load-theme 'gruvbox-dark-hard t))
;; Dracula theme
;; (use-package dracula-theme
;;   :init (load-theme 'dracula t))
;; Nord theme
;; (use-package nord-theme
;;   :init (load-theme 'nord t))
;; Zenburn theme
;; (use-package zenburn-theme
;;   :init (load-theme 'zenburn t))
;; (load-theme 'deeper-blue t)

;; Modus theme configuration
;; modus-themes-toggle toggles between light and dark themes
;; (setq modus-themes-mode-line '(borderless)
;;       modus-themes-region '(bg-only)
;;       modus-themes-completions 'opinionated
;;       modus-themes-bold-constructs t
;;       modus-themes-italic-constructs t
;;       modus-themes-fringes 'subtle
;;       modus-themes-tabs-accented t
;;       modus-themes-paren-match '(bold intense)
;;       modus-themes-syntax '(alt-syntax)
;;       modus-themes-headings
;;       '((1 . (rainbow overline background 1.4))
;; 	(2 . (rainbow background 1.3))
;; 	(3 . (rainbow bold 1.2))
;; 	(t . (semilight 1.1)))
;;       modus-themes-scale-headings t
;;       modus-themes-org-blocks 'tinted-background)
;; (load-theme 'modus-vivendi t)

(use-package all-the-icons)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  ;; (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
  ;;       doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-palenight t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-atom" for more minimal icon theme
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Counsel for fuzzy auto-completions
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

;; Ivy for completions
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; prescient.el
;; Sorting and filtering selections based on use recency, frequency and configurable rules
(use-package prescient
  :after counsel
  :config
  (setq prescient-sort-length-enable nil)
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1)
  (setq ivy-prescient-retain-classic-highlighting t))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))

(use-package avy
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line)
  :bind
  ("C-: c" . avy-goto-char)
  ("C-: w" . avy-goto-word-0)
  ("C-: l" . avy-goto-line))


(use-package expand-region
  :bind (("M-[" . er/expand-region)
         ("C-(" . er/mark-outside-pairs)))

;; Requires M-x all-the-icons-install-fonts to show icons correctly

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15))

;; Use different colors for nested parens
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

;; sets the text background to the color mentioned. For example: #0000ff, #ff0000
(use-package rainbow-mode
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         web-mode))

;; Helpful visual auto-completion for keywords
(use-package which-key
  :init (which-key-mode)
  :diminish
  :config
  (setq which-key-idle-delay 0.3))

;; Improved helpful pages
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra)
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; Org-projectile configuration
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Org mode configuration
;; Ensure the latest org version is used
(defun sm/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . sm/org-mode-setup)
  :pin gnu
  :ensure org-contrib
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
         (:map org-mode-map
               ("C-M-j" . org-next-visible-heading)
               ("C-M-k" . org-previous-visible-heading)
               ("M-j" . org-metadown)
               ("M-k" . org-metaup)))
  :config
  ;; Custom org mode settings
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t
	org-agenda-start-with-log-mode t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-startup-folded 'content
      	org-log-done 'time
	org-log-into-drawer t
        
	org-directory "~/Dropbox/org/"
	org-default-notes-file "Inbox.org"
	org-agenda-files '("Work.org" "Personal.org" "Habits.org")
	org-refile-targets
	'(("Archive.org" :maxlevel . 1))

        org-tag-alist
        '((:startgroup)
         ; Put mutually exclusive tags here
          (:endgroup)
          ("@home" . ?h)
          ("@work" . ?w))

        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "DOING(i)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
	 ;; (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)"))

        org-todo-keyword-faces
        '(("NEXT" :foreground "orange" :weight bold)
          ("DOING" :foreground "blue" :weight bold)
          ("WAITING" :foreground "orange" :weight bold)
          ("HOLD" :foreground "magenta" :weight bold))

        ;; allows changing the from any task state to any other state with C-c C-r KEY
        org-use-fast-todo-selection t
        
        org-capture-templates
	`(("c" "Inbox Capture" entry (file+headline "Inbox.org" "Tasks")
           "* TODO %?\n %U\n" :empty-lines 1 :kill-buffer t)
          ("t" "Tasks / Projects")
          ("tw" "Work Task" entry (file+olp "Work.org" "Tasks")
           "* TODO %?\n %U\n" :empty-lines 1 :kill-buffer t)
          ("tp" "Personal Task" entry (file+olp "Personal.org" "Tasks")
           "* TODO %?\n %U\n" :empty-lines 1 :kill-buffer t)
	  ("n" "Notes" entry (file+olp+datetree "Notes.org")
	   "* %^{Description} %^g %?\nAdded: %U" :empty-lines 1 :kill-buffer t)
          ("j" "Journal" entry (file+olp+datetree "Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           :clock-in :clock-resume :empty-lines 1)
          ("m" "Meeting" entry (file+olp+datetree "Notes.org")
           "\n* %<%I:%M %p> - %^{Meeting title} :meeting:\n\n%?\n\n"
           :clock-in :clock-resume :empty-lines 1)
          ("b" "Books" entry (file+headline "Books.org" "Books")
           "* TODO %?\n %U\n" :empty-lines 1 :kill-buffer t))

        org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
              ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/DOING" ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
              ((org-agenda-overriding-header "Next Tasks")))))


          ;; ("W" "Work Tasks" tags-todo "+work")

          ;; Low-effort next actions
          ;; ("e;; " tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ;; ((org-agenda-overriding-header "Low Effort Tasks")
           ;;  (org-agenda-max-todos 20)
           ;;  (org-agenda-files org-agenda-files)))

          ;; ("w" "Workflow Status"
          ;;  ((todo "WAIT"
          ;;         ((org-agenda-overriding-header "Waiting on External")
          ;;          (org-agenda-files org-agenda-files)))
          ;;   (todo "REVIEW"
          ;;         ((org-agenda-overriding-header "In Review")
          ;;          (org-agenda-files org-agenda-files)))
          ;;   (todo "PLAN"
          ;;         ((org-agenda-overriding-header "In Planning")
          ;;          (org-agenda-todo-list-sublevels nil)
          ;;          (org-agenda-files org-agenda-files)))
          ;;   (todo "BACKLOG"
          ;;         ((org-agenda-overriding-header "Project Backlog")
          ;;          (org-agenda-todo-list-sublevels nil)
          ;;          (org-agenda-files org-agenda-files)))
          ;;   (todo "READY"
          ;;         ((org-agenda-overriding-header "Ready for Work")
          ;;          (org-agenda-files org-agenda-files)))
          ;;   (todo "ACTIVE"
          ;;         ((org-agenda-overriding-header "Active Projects")
          ;;          (org-agenda-files org-agenda-files)))
          ;;   (todo "COMPLETED"
          ;;         ((org-agenda-overriding-header "Completed Projects")
          ;;          (org-agenda-files org-agenda-files)))
          ;;   (todo "CANC"
          ;;         ((org-agenda-overriding-header "Cancelled Projects")
          ;;          (org-agenda-files org-agenda-files)))))
          ))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  )

;; Org-bullets configuration
(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Change size for different levels of org headlines
(require 'org-indent)
(dolist (face '((org-level-1 . 1.25)
		(org-level-2 . 1.25)
		(org-level-3 . 1.22)
		(org-level-4 . 1.2)
		(org-level-5 . 1.2)
		(org-level-6 . 1.2)
		(org-level-7 . 1.2)
		(org-level-8 . 1.2)))
  (set-face-attribute (car face) nil :weight 'regular :height (cdr face)))

;; Ensure that anything that should be fixed-pitch in Org files appears that way.
;; Eveything else will be variable-pitch
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-formula nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(use-package org-appear
  :hook (org-mode . org-appear-mode))

;; ;; Send notifications for org mode tasks
;; (use-package org-alert
;;   :custom (alert-default-style 'message)
;;   :config
;;   (setq org-alert-interval 300
;;         org-alert-notification-title "Org Alert Reminder!")
;;   (org-alert-enable))

;; (use-package org-wild-notifier
;;   :after org
;;   :config
;;   ;; Make sure we receive notifications for non-TODO events
;;   ;; like those synced from Google Calendar
;;   (setq org-wild-notifier-keyword-whitelist nil)
;;   (setq org-wild-notifier-notification-title "Agenda Reminder")
;;   (setq org-wild-notifier-alert-time 15)
;;   (org-wild-notifier-mode))

;; (use-package org-notify
;;   :ensure nil
;;   :after org
;;   :config
;;   (org-notify-start))

;; Add margins to Org mode docs
(defun sm/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . sm/org-mode-visual-fill))

;; Org babel
(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)))

(setq org-confirm-babel-evaluate nil)

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; Auto-tangle configuration file when saving it
(defun sm/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/emacs.d/Emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'sm/org-babel-tangle-config)))


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

;; Org-roam configuration
(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (concat org-directory "roam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup)
  (require 'org-roam-protocol))

(use-package org-projectile
  :bind (("C-c n p" . org-projectile-project-todo-completing-read))
  :config
  (progn
    (setq org-projectile-projects-file
          (concat org-directory "projects.org"))
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates)))

;; Set default connection mode to SSH
(setq tramp-default-method "ssh")

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

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

;; Magit forge: allows to work with github and gitlab (e.g. pulling issues, creating pull requests, etc)
;; Requires setting up a token for the Github API
;; TODO: Set it up
(use-package forge)

;; IDE configuration
;; Header breadcrumb
(defun sm/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . sm/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

;; needs to install LSP for the specific languages first
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package python-mode
  :ensure nil
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package rust-mode)

(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)


;; IDE-like auto-completions 
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map
	("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Comment code in any language
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package yasnippet)

;; Configure the terminal
(use-package term
  :config
  (setq explicit-shell-fine-name "bash")
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

;; sudo apt-get install cmake cmake-data libtool
;; TODO: Fix installation of dependencies
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

;; Configuring dired
;; Require to mark by extension
(require 'dired-x)
;; Keep only one dired buffer
(use-package dired-single)

(use-package dired
  :ensure nil
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

(use-package dired-open
  :config
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extension '(("png" . "feh")
			       ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind
  (:map dired-mode-map
	("H" . dired-hide-dotfiles-mode)))

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (defun sm/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.3)
                    (markdown-header-face-2 . 1.25)
                    (markdown-header-face-3 . 1.2)
                    (markdown-header-face-4 . 1.2)
                    (markdown-header-face-5 . 1.2)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

  (defun sm/markdown-mode-hook ()
    (sm/set-markdown-header-font-sizes))

  (add-hook 'markdown-mode-hook 'sm/markdown-mode-hook))

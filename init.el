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

(set-face-attribute 'default nil :font "Source Code Pro" :height 120)
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height 115)
(set-face-attribute 'variable-pitch nil :font "Source Sans Pro"  :height 125 :weight 'regular)

;; make line numbers visible
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line number for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		treemacs-mode-hook))
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
(setq modus-themes-mode-line '(borderless)
      modus-themes-region '(bg-only)
      modus-themes-completions 'opinionated
      modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-fringes 'subtle
      modus-themes-tabs-accented t
      modus-themes-paren-match '(bold intense)
      modus-themes-syntax '(alt-syntax)
      modus-themes-headings
      '((1 . (rainbow overline background 1.4))
	(2 . (rainbow background 1.3))
	(3 . (rainbow bold 1.2))
	(t . (semilight 1.1)))
      modus-themes-scale-headings t
      modus-themes-org-blocks 'tinted-background)
(load-theme 'modus-vivendi t)

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

;; Requires M-x nerd-icons-install-fonts to show icons correctly
(use-package nerd-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15))

;; Use different colors for nested parens
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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
	 ("C-c c" . org-capture))
  :config
  ;; Custom org mode settings
  ;; Save Org buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t
	org-agenda-start-with-log-mode t
	org-log-done t
	org-log-into-drawer t
	org-directory "~/Dropbox/org/"
	org-default-notes-file "inbox.org"
	org-agenda-files '("work.org" "home.org" "Test.org")
	org-refile-targets
	'(("archive.org" :maxlevel . 1))
	org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)"))
	org-capture-templates
	'(("t" "Task" entry (file+headline "inbox.org" "Tasks")
           "* TODO %?\n" :jump-to-captured t :kill-buffer t)
	  ("n" "Notes" entry (file+olp+datetree "inbox.org")
	   "* %^{Description} %^g %?\nAdded: %U" :jump-to-captured t :kill-buffer t))
	org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
              ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
              ((org-agenda-overriding-header "Next Tasks")))))


          ("W" "Work Tasks" tags-todo "+work")

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

          ("w" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on External")
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "In Review")
                   (org-agenda-files org-agenda-files)))
            (todo "PLAN"
                  ((org-agenda-overriding-header "In Planning")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
                  ((org-agenda-overriding-header "Project Backlog")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Ready for Work")
                   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Completed Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Cancelled Projects")
                   (org-agenda-files org-agenda-files))))))))

;; Org-bullets configuration
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Change size for different levels of org headlines
(require 'org-indent)
(dolist (face '((org-level-1 . 1.1)
		(org-level-2 . 1.05)
		(org-level-3 . 1.02)
		(org-level-4 . 1.0)
		(org-level-5 . 1.0)
		(org-level-6 . 1.0)
		(org-level-7 . 1.0)
		(org-level-8 . 1.0)))
  (set-face-attribute (car face) nil :weight 'regular :height (cdr face)))

;; Ensure that anything that should be fixed-pitch in Org files appears that way.
;; Eveything else will be variable-pitch
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

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

;; Syntax highlighting for code blocks in Org mode
(setq org-src-fontify-natively t)

(use-package org-projectile
  :bind (("C-c n p" . org-projectile-project-todo-completing-read))
  :config
  (progn
    (setq org-projectile-projects-file
          (concat org-directory "projects.org"))
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates)))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

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
  :hook (python-mode . lsp-deferred))

(use-package rust-mode)

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

(use-package markdown-mode)

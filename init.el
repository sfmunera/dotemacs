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

;; (set-face-attribute 'default nil :font "Fira Code Retina" :height 280)

;; make line numbers visible
(global-display-line-numbers-mode 1)

(setq blink-cursor-mode nil)

;; quick access to recently edited files
(recentf-mode 1)
;; TODO: add binding to recentf-open-files

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
(use-package counsel)

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

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15))
;; TODO: fix icons in modeline



(use-package lsp-mode
  :hook (prog-mode . lsp))

;; Org mode configuration
;; Ensure the latest org version is used
(use-package org
  :pin gnu
  :ensure org-contrib
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  ;; Custom org mode settings
  (setq org-directory "~/Dropbox/org/")
  (setq org-hide-emphasis-markers t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-default-notes-file "inbox.org")
  (setq org-agenda-files (list "work.org"
			       "home.org"))
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "inbox.org" "Tasks")
           "* TODO %?\n" :jump-to-captured t :kill-buffer t)
	  ("n" "Notes" entry (file+olp+datetree "inbox.org")
	   "* %^{Description} %^g %?\nAdded: %U" :jump-to-captured t :kill-buffer t))))

;; Org-bullets configuration
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

;; Org-roam configuration
(use-package org-roam
  :ensure t
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

;; Org-projectile configuration
(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package org-projectile
  :ensure t
  :bind (("C-c n p" . org-projectile-project-todo-completing-read))
  :config
  (progn
    (setq org-projectile-projects-file
          (concat org-directory "projects.org"))
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates)))

(use-package yasnippet)
(use-package lsp-treemacs)
(add-hook 'python-mode-hook
          (lambda ()
            (unless (treemacs-get-local-window)
              (treemacs))))

(use-package helm-lsp)
(use-package rust-mode)
(use-package python-mode)

(use-package helm
  :config (helm-mode 1))

(use-package company
  :config (add-hook 'after-init-hook 'global-company-mode))

(use-package magit)

(use-package markdown-mode
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f681100b27d783fefc3b62f44f84eb7fa0ce73ec183ebea5903df506eb314077" default))
 '(package-selected-packages
   '(markdown-preview-mode zenburn-theme yasnippet use-package solarized-theme rust-mode python-mode org-roam org-projectile org-contrib org-bullets nord-theme magit lsp-treemacs impatient-mode helm-lsp gruvbox-theme grip-mode dracula-theme company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

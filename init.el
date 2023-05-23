(server-start)

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
(use-package dracula-theme
  :init (load-theme 'dracula t))
;; Nord theme
;; (use-package nord-theme
;;   :init (load-theme 'nord t))
;; Zenburn theme
;; (use-package zenburn-theme
;;   :init (load-theme 'zenburn t))

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
  (setq org-directory "/home/sfmunera/Dropbox/org/")
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
(use-package markdown-preview-mode
  :ensure t)


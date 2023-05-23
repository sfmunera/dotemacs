(server-start)

;; Package system and settings
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Solarized theme
(use-package solarized-theme
  :init (load-theme 'solarized-dark t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-projectile org-roam org-bullets org-contrib use-package solarized-theme lsp-mode yasnippet lsp-treemacs helm-lsp projectile rust-mode python-mode org-mode magit helm company markdown-mode grip-mode))
 '(warning-suppress-types '((lsp-mode) (lsp-mode) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package lsp-mode
  :hook (prog-mode . lsp))

;; Org mode configuration
;; Ensure the latest org version is used
(use-package org
  :pin gnu
  :ensure org-contrib
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
	   "* %^{Description} %^g %?\nAdded: %U" :jump-to-captured t :kill-buffer t)))
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture))

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
(use-package grip-mode)

(defun org-preview-as-markdown ()
  "Export the current Org buffer to Markdown and preview in `markdown-mode'."
  (interactive)
  (let* ((org-buffer (current-buffer))
         (md-buffer (get-buffer-create
                     (concat (buffer-name org-buffer) ".md"))))
    (with-current-buffer md-buffer
      (erase-buffer))
    (org-md-export-as-markdown nil nil md-buffer nil)
    (delete-other-windows)  ;; Optional: delete all other windows
    (split-window-right)    ;; Split window vertically
    (other-window 1)        ;; Move to the new window
    (switch-to-buffer md-buffer)  ;; Show md buffer in the new window
    (markdown-preview)))    ;; Start markdown preview

(define-key org-mode-map (kbd "C-c m") 'org-preview-as-markdown)

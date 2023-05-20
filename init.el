(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package solarized-theme
  :init (load-theme 'solarized-dark t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(use-package solarized-theme lsp-mode yasnippet lsp-treemacs helm-lsp projectile rust-mode python-mode org-mode magit helm company markdown-mode grip-mode))
 '(warning-suppress-types '((lsp-mode) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package lsp-mode
  :hook (prog-mode . lsp))

(use-package org
  :config
  (setq org-agenda-files (list "~/Dropbox/org/work.org"
                               "~/Dropbox/org/home.org"))
  (define-key global-map "\C-cc" 'org-capture)
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Dropbox/org/gtd.org" "Tasks")
           "* TODO %?\n  %i\n  %a"))))

(use-package yasnippet)
(use-package lsp-treemacs)
(use-package helm-lsp)
(use-package projectile)
(use-package rust-mode)
(use-package python-mode)

(use-package helm
  :config (helm-mode 1))

(use-package company
  :config (add-hook 'after-init-hook 'global-company-mode))

(use-package magit)

(use-package markdown-mode)
(use-package grip-mode)

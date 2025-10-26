;;; my-org-babel.el --- Org Babel configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Org babel and code block configurations

;;; Code:

(require 'org)

;;;; Jupyter Integration

(use-package jupyter
  :after (org))

;;;; Org Babel Configuration

;; Auto-detect Python executable
(setq org-babel-python-command
      (or (executable-find "python3")
          (executable-find "python")
          "python3")) ; fallback if neither found
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)))
;;(org-babel-jupyter-override-src-block "python")

(require 'ob-python)

;; Only disable confirmation for trusted languages
;; Set to t to confirm all, or use lambda to selectively trust languages
(setq org-confirm-babel-evaluate
      (lambda (lang body)
        "Confirm evaluation for all languages except trusted ones."
        (not (member lang '("emacs-lisp" "shell" "python")))))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;;;; Source Block Templates

;; shortcut for source blocks: e.g. <el TAB autocompletes the source block
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("rs" . "src rust"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

(provide 'my-org-babel)
;;; my-org-babel.el ends here

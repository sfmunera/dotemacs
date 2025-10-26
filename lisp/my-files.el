;;; my-files.el --- File management (Dired) -*- lexical-binding: t -*-

;;; Commentary:
;;File management (Dired)

;;; Code:

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
         ("C-+" . dired-create-empty-file)
         ("% l" . prot-dired-limit-regexp)
         ("C-c g" . prot-dired-grep-marked-files)))
  :custom
  ;; Keep only one dired buffer
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  (dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
  (dired-make-directory-clickable t) ; Emacs 29.1
  (dired-free-space nil) ; Emacs 29.1
  (dired-mouse-drag-files t) ; Emacs 29.1
  (dired-create-destination-dirs 'ask) ; Emacs 27
  (dired-create-destination-dirs-on-trailing-dirsep t) ; Emacs 29
  (dired-guess-shell-alist-user '(("\\.png" "feh")
                                   ("\\.mkv" "mpv")))
  ;;  (dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
  :config
  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls
        (setq dired-use-ls-dired t
              insert-directory-program gls
              dired-listing-switches "-aBhl  --group-directories-first")))))

(use-package dired-aux
  :straight nil
  :after dired
  :bind
  ( :map dired-mode-map
    ("C-<return>" . dired-do-open)) ; Emacs 30
  :custom
  (dired-isearch-filenames 'dwim)
  (dired-create-destination-dirs 'ask) ; Emacs 27
  (dired-do-revert-buffer t) ; Emacs 28
  (dired-create-destination-dirs-on-trailing-dirsep t)) ; Emacs 29

(use-package dired-subtree
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :custom
  (dired-subtree-use-backgrounds nil))

(use-package dired-hide-dotfiles
  :after dired
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind
  (:map dired-mode-map
        ("." . dired-hide-dotfiles-mode)))

(use-package dired-x
  :straight nil
  :after dired
  :custom
  (dired-clean-up-buffers-too t)
  (dired-clean-confirm-killing-deleted-buffers t))

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
  :hook (dired-mode . nerd-icons-dired-mode))

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

(provide 'my-files)
;;; my-files.el ends here

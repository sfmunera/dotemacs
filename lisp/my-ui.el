;;; my-ui.el --- User interface configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Visual appearance, themes, fonts, and UI elements

;;; Code:


;;;; Basic UI configuration
;;;; Note: tool-bar, tooltip, menu-bar, and visible-bell are set in early-init.el

;; Additional UI customizations
;;(scroll-bar-mode -1)
(set-fringe-mode 10)

;; Don't show UI dialogs when prompting
(setq use-dialog-box nil)

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

;; (use-package modus-themes
;;   :bind (("<f7>" . modus-themes-toggle)
;;          ("C-<f7>" . modus-themes-select))
;;   :config
;;   (setq modus-themes-italic-constructs t
;;         modus-themes-bold-constructs t
;;         modus-themes-common-palette-overrides nil
;;         ;;'((builtin red-cooler))
;;         modus-themes-mixed-fonts t
;;         modus-themes-variable-pitch-ui t
;;         modus-themes-disable-other-themes t
;;         modus-themes-to-toggle '(modus-operandi modus-vivendi)
;;         modus-themes-headings
;;         '((agenda-structure . (variable-pitch light 2.2))
;;           (agenda-date . (variable-pitch regular 1.3))
;;           (t . (regular 1.15)))
;;         ;;'((1 1.3) (2 1.2) (3 1.1))
;;         )
;;   ;;:init
;;   ;;(load-theme 'modus-operandi :no-confirm-loading)
;;   )

(use-package ef-themes
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :bind
  (("<f5>"   . modus-themes-toggle)
   ("C-<f5>" . modus-themes-select)
   ("M-<f5>" . modus-themes-rotate))
  :config
  (setq modus-themes-headings ; read the manual's entry of the doc string
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
          (t . (variable-pitch 1.1)))
        modus-themes-variable-pitch-ui t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-disable-other-themes t
        modus-themes-to-toggle '(ef-maris-light ef-maris-dark))
  (modus-themes-load-theme 'ef-maris-light))

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

(provide 'my-ui)
;;; my-ui.el ends here

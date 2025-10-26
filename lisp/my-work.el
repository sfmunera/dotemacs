;;; my-work.el --- Work-specific configs -*- lexical-binding: t -*-

;;; Commentary:
;; Work-specific configs

;;; Code:

(let ((work-config-file (expand-file-name "work-config.el" user-emacs-directory)))
  (when (file-exists-p work-config-file)
    (condition-case err
        (load work-config-file)
      (error (message "Failed to load work config: %s" err)))))

(provide 'my-work)
;;; my-work.el ends here

;;; my-tramp.el --- TRAMP remote file editing -*- lexical-binding: t -*-

;;; Commentary:
;; TRAMP remote file editing

;;; Code:

;;; TRAMP

;; Set default connection mode to SSH
(setq tramp-default-method "ssh")
(defun sudo ()
  "Use TRAMP to `sudo' the current buffer."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

(provide 'my-tramp)
;;; my-tramp.el ends here

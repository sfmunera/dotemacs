;;; my-tramp.el --- TRAMP remote file editing -*- lexical-binding: t -*-

;;; Commentary:
;; TRAMP remote file editing

;;; Code:

;;; TRAMP

;; Set default connection mode to SSH
(setq tramp-default-method "ssh")

(defun sudo ()
  "Use TRAMP to `sudo' the current buffer.
Prompts to save if buffer is modified."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (when (and (buffer-modified-p)
             (not (y-or-n-p "Buffer modified. Save before sudo? ")))
    (user-error "Aborted"))
  (when (buffer-modified-p)
    (save-buffer))
  (let ((file buffer-file-name))
    (find-alternate-file
     (concat "/sudo:root@localhost:" file))))

(provide 'my-tramp)
;;; my-tramp.el ends here

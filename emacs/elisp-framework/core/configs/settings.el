;;; settings.el --- Settings definitions.
;;; Commentary:
;;; Code:

;; Turn off backups and autosaves. @todo: Improve this
(setq create-lockfiles nil)
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;; Create cache
(core:mkdir dir-cache)
(core:mkdir dir-backups)
(core:mkdir dir-autosaves)

(global-auto-revert-mode t) ; Refresh file if modified.

(provide 'settings)

;;; settings.el ends here

(use-package dired
  :straight nil
  :bind (:map dired-mode-map ([mouse-2] . dired-find-file))
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-use-ls-dired nil)
 (dired-mouse-drag-files t))

(use-package all-the-icons-dired
  :after (all-the-icons)
  :if (display-graphic-p)
  :hook
  (dired-mode . (lambda () (interactive)
                  (unless (file-remote-p default-directory)
                    (all-the-icons-dired-mode)))))

(use-package dired-subtree
  :init
  (advice-add 'dired-subtree-toggle
              :after (lambda () (interactive)
                       (when all-the-icons-dired-mode (revert-buffer)))))

(provide 'acf-project)

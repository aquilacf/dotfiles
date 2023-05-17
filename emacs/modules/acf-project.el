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

;; Not working as expected
(use-package dired-subtree
  :init
  (advice-add 'dired-subtree-toggle
              :after (lambda () (interactive)
                       (when all-the-icons-dired-mode (revert-buffer)))))


(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar)
  :bind (("§" . dired-sidebar-toggle-sidebar))
  :custom
  (dired-sidebar-subtree-line-prefix "  ")
  ;; (dired-sidebar-theme 'vscode)
  ;; (dired-sidebar-use-term-integration t)
  ;; (dired-sidebar-use-custom-font t)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))
              (display-line-numbers-mode -1)
              ))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands))

(provide 'acf-project)

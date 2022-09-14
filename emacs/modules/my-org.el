(use-package org)
(setq org-element-cache-persistent nil)


(use-package org-bullets)

(defun my/org-setup ()
  (org-bullets-mode 1)
  (org-indent-mode 1))


(use-package org-present)
(use-package visual-fill-column)
(custom-set-variables '(visual-fill-column-width 110)
		      '(visual-fill-column-center-text t))

(defun my/org-present-start ()
  (org-display-inline-images)

  (visual-fill-column-mode 1)
  (visual-line-mode 1)

  (git-gutter-mode 0))

(defun my/org-present-end ()
  (org-remove-inline-images)

  (visual-fill-column-mode 0)
  (visual-line-mode 0)
  
  (git-gutter-mode 1))

(defun my/org-present-prepare-slide (buffer-name heading)
  (org-overview)
  (org-show-entry)
  (org-show-children))

(add-hook 'org-mode-hook 'my/org-setup)
(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-quit-hook 'my/org-present-end)
(add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide)

(provide 'my-org)

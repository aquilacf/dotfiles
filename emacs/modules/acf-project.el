;;; acf-project.el --- Project/File management module.

;;; Commentary:
;; This module is used for project/file management.

;; -*- lexical-binding: t -*-

;;; Code:

(require 'ls-lisp)

(use-package dired
  :straight nil
  :bind (:map dired-mode-map ([mouse-2] . dired-find-file))
  :hook (dired-mode . dired-omit-mode)
  :custom
  (ls-lisp-use-insert-directory-program nil)
  (ls-lisp-dirs-first t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-use-ls-dired nil)
  (dired-mouse-drag-files t)
  (dired-omit-files "^\\.[^.].*"))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-subtree
  :defer 2
  :bind (:map dired-mode-map ("TAB" . dired-subtree-cycle))
  :custom (dired-subtree-use-backgrounds nil))

(use-package dired-sidebar
  :bind (("§" . dired-sidebar-toggle-sidebar))
  :hook (dired-sidebar-mode . (lambda () (display-line-numbers-mode -1) (setq-local mode-line-format nil)))
  :custom
  (dired-sidebar-subtree-line-prefix "  "))


(provide 'acf-project)

;;; acf-project.el ends here

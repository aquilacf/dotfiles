;;; acf-project.el --- Project/File management module.

;;; Commentary:
;; This module is used for project/file management.

;; -*- lexical-binding: t -*-

;;; Code:

(require 'ls-lisp)

(use-package dired
  :bind (:map dired-mode-map ([mouse-2] . dired-find-file))
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-listing-switches "-alh")
  (ls-lisp-use-insert-directory-program nil)
  (ls-lisp-dirs-first t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-use-ls-dired nil)
  (dired-mouse-drag-files t)
  (dired-omit-files "^\\.[^.].*"))

(use-package dired-subtree
  :ensure t
  :defer 2
  :bind (:map dired-mode-map ("TAB" . dired-subtree-cycle))
  :custom (dired-subtree-use-backgrounds nil))

(use-package dired-sidebar
  :ensure t
  :bind (("§" . dired-sidebar-toggle-sidebar))
  :hook (dired-sidebar-mode . acf/dired-sidebar/setup)
  :init
  (defun acf/dired-sidebar/setup ()
    (display-line-numbers-mode -1)
    (setq-local mode-line-format nil))
  :custom
  (dired-sidebar-subtree-line-prefix "  "))



;; (custom-set-variables
;;  '(grep-find-ignored-directories . ("folder-to-ignore"))
;;  '(grep-find-ignored-files . ("file-to-ignore")))


(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "tmp")
     (add-to-list 'grep-find-ignored-directories "node_modules")
     (add-to-list 'grep-find-ignored-directories ".bundle")
     (add-to-list 'grep-find-ignored-directories "auto")
     (add-to-list 'grep-find-ignored-directories "elpa")))





(provide 'acf-project)

;;; acf-project.el ends here

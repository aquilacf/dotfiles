;; -*- lexical-binding: t -*-


;; Projects
(setq project-list-file (concat DIR_CACHE "projects"))


;;;;;;;;;;;;
;; Gutter ;;
;;;;;;;;;;;;
(use-package git-gutter
  :defer 1
  :custom
  (git-gutter:added-sign " +")
  (git-gutter:deleted-sign " -")
  (git-gutter:modified-sign " \u2502")
  (git-gutter:separator-sign nil)
  (git-gutter:unchanged-sign nil)
  (git-gutter:lighter nil)
  (git-gutter:window-width 2)
  (git-gutter:visual-line t)
  (git-gutter:update-interval 0)

  :config
  (set-face-attribute 'git-gutter:added nil :inherit 'default :foreground "green1" :background 'unspecified)
  (set-face-attribute 'git-gutter:deleted nil :inherit 'default :foreground "red" :background 'unspecified)
  (set-face-attribute 'git-gutter:modified nil :inherit 'default :foreground "yellow" :background 'unspecified)
  (set-face-attribute 'git-gutter:unchanged nil :inherit 'default :foreground 'unspecified :background 'unspecified)
  (global-git-gutter-mode))


;;;;;;;;;;;
;; Magit ;;
;;;;;;;;;;;
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (transient-history-file (concat DIR_CACHE "transient/history.el"))
  (transient-values-file (concat DIR_CACHE "transient/values.el"))
  (transient-levels-file (concat DIR_CACHE "transient/levels.el")))


;;;;;;;;;;;
;; Dired ;;
;;;;;;;;;;;
(customize-set-variable 'dired-kill-when-opening-new-dired-buffer t)
(with-eval-after-load 'dired
  (define-key dired-mode-map [mouse-2] 'dired-find-file))


(provide 'my-project)

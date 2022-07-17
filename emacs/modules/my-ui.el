;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;
;; Treemacs ;;
;;;;;;;;;;;;;;
(use-package treemacs
  :bind ("§" . treemacs)
  :custom
  (treemacs-persist-file (concat DIR_CACHE "treemacs-persist"))
  (treemacs-last-error-persist-file (concat DIR_CACHE "treemacs-persist-at-last-error"))
  :config
  (treemacs-git-commit-diff-mode)
  (treemacs-filewatch-mode)
  (treemacs-project-follow-mode)
  (treemacs-follow-mode))

(use-package treemacs-projectile
  :after (treemacs projectile))



(use-package rainbow-mode
  :defer 5
  :config (rainbow-mode))



(provide 'my-ui)

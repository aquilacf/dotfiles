;; -*- lexical-binding: t -*-


;; Parenthesis
;(set-face-attribute 'show-paren-match nil :foreground 'unspecified :background 'unspecified :weight 'extra-bold :underline t)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)
(show-paren-mode t)


(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq use-dialog-box nil)

;; Line numbers
(setq-default display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(global-visual-line-mode)

(column-number-mode)
(delete-selection-mode t)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

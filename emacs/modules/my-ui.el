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

;;;;;;;;;;;;;;;;;;;
;; All the icons ;;
;;;;;;;;;;;;;;;;;;;
(use-package all-the-icons
  :if (display-graphic-p)
  :demand t)

(use-package rainbow-mode
  :defer 5
  :config (rainbow-mode))

(provide 'my-ui)

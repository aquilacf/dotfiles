;;; acf-shell.el --- Shell/Terminal module.

;;; Commentary:
;; This module contains all the shell/terminal related configuration.

;; -*- lexical-binding: t -*-

;;; Code:

;; (use-package xterm-color
;;   :disabled
;;   :custom
;;   (compilation-environment '("TERM=xterm-256color"))
;;   :init
;;   (defun acf/compilation-filter (f proc string)
;;     (funcall f proc (xterm-color-filter string)))
;;   :config
;;   (advice-add 'compilation-filter :around #'acf/compilation-filter))


(use-package eshell-syntax-highlighting :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package eat
  :straight (eat
             :type git
             :host codeberg
             :repo "akib/emacs-eat"
             :files ("*.el" ("term" "term/*.el") "*.texi"
                     "*.ti" ("terminfo/e" "terminfo/e/*")
                     ("terminfo/65" "terminfo/65/*")
                     ("integration" "integration/*")
                     (:exclude ".dir-locals.el" "*-tests.el")))
  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package eshell
  ;;  :hook (eshell-mode . acf/eshell-filter)
  ;; :init
  ;; (defun acf/eshell-filter ()
  ;;      (add-hook 'eshell-before-prompt-hook   (lambda () (setq xterm-color-preserve-properties t)))
  ;; (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  ;; (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  ;;      (setenv "TERM" "xterm-256color"))
  :custom
  (eshell-banner-message "")
  (eshell-hist-ignoredups t))



(provide 'acf-shell)

;;; acf-shell.el ends here

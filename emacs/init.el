;;; init.el --- Emacs init.

;;; Commentary:
;; Sets up elpaca.

;; -*- lexical-binding: t -*-
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;;; Code:
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'acf-defaults)
(require 'acf-editing)
(require 'acf-ui)
(require 'acf-completion)
(require 'acf-project)
;; (require 'acf-programming)
;; (require 'acf-pdf)
;; (require 'acf-shell)
;; (require 'acf-org)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(auto-save-interval 0)
 '(auto-save-list-file-name nil t)
 '(auto-save-timeout 0)
 '(auto-window-vscroll nil t)
 '(backup-inhibited t t)
 '(create-lockfiles nil)
 '(fast-but-imprecise-scrolling nil)
 '(global-auto-revert-non-file-buffers t)
 '(hscroll-margin 1)
 '(hscroll-step 1)
 '(isearch-lazy-count t)
 '(lazy-count-prefix-format nil)
 '(lazy-count-suffix-format "   (%s/%s)")
 '(make-backup-files nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1)))
 '(package-selected-packages
   '(highlight-indent-guides diff-hl dired-sidebar dired-subtree kind-icon corfu orderless consult marginalia vertico rainbow-delimiters doom-modeline doom-themes emojify evil-collection evil no-littering))
 '(recentf-auto-cleanup 'never)
 '(recentf-max-menu-items 20)
 '(recentf-max-saved-items 20)
 '(ring-bell-function 'ignore)
 '(scroll-conservatively 101)
 '(scroll-down-aggressively 0.01)
 '(scroll-margin 1)
 '(scroll-preserve-screen-position t)
 '(scroll-step 1)
 '(scroll-up-aggressively 0.01)
 '(show-paren-delay 0)
 '(show-paren-style 'parenthesis)
 '(use-dialog-box nil)
 '(visible-bell nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

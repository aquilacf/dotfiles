(use-package no-littering :demand t)

;; File handling
(global-auto-revert-mode)
(custom-set-variables
 '(global-auto-revert-non-file-buffers t)
 '(create-lockfiles nil))

;; Backups
(custom-set-variables
 '(backup-inhibited t)
 '(make-backup-files nil))

;; Autosaves
(customize-set-variable 'auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(custom-set-variables
 '(auto-save-list-file-name nil)
 '(auto-save-default nil)
 '(auto-save-interval 0)
 '(auto-save-timeout 0))

(customize-set-variable 'warning-minimum-level :error)

;; Recent files
(custom-set-variables
 '(recentf-max-menu-items 20)
 '(recentf-max-saved-items 20)
 '(recentf-auto-cleanup 'never))

(recentf-mode)
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)

;; Scroll
(custom-set-variables
 '(scroll-preserve-screen-position t)
 ;; Vertical Scroll
 '(scroll-step 1)
 '(scroll-margin 1)
 '(scroll-conservatively 101)
 '(scroll-up-aggressively 0.01)
 '(scroll-down-aggressively 0.01)
 '(auto-window-vscroll nil)
 '(fast-but-imprecise-scrolling nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1)))
 '(mouse-wheel-progressive-speed nil)
 ;; Horizontal Scroll
 '(hscroll-step 1)
 '(hscroll-margin 1))


;; History
(savehist-mode)
(save-place-mode)

(customize-set-variable 'use-short-answers t)
(customize-set-variable 'server-client-instructions nil)

(customize-set-variable 'cursor-type 'bar)




(defun acf/load-mac-defaults ()
  (require 'acf-osx)
  (acf/osx/update-path-from-zshell))


(if IS_MAC (acf/load-mac-defaults))


(provide 'acf-defaults)

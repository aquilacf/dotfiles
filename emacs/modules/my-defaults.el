;; -*- lexical-binding: t -*-

(defun my/save-all-buffers ()
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'my/save-all-buffers)

(customize-set-variable 'warning-minimum-level :emergency)


;; Recent files
(custom-set-variables '(recentf-max-menu-items 20)
		      '(recentf-max-saved-items 20)
		      '(recentf-save-file (concat DIR_CACHE "recentf")))
(recentf-mode t)

;; Scroll
(setq auto-window-vscroll nil)
(custom-set-variables '(fast-but-imprecise-scrolling t)
		      '(scroll-conservatively 101)
		      '(scroll-margin 0)
		      '(scroll-preserve-screen-position t))

;; Long lines
(setq-default bidi-display-reordering 'left-to-right
	      bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode t)


;; History
(custom-set-variables '(history-length 30)
		      '(savehist-file (concat DIR_CACHE "history"))
		      '(url-history-file (concat DIR_CACHE "url-history")))
(savehist-mode t)

;; Save cursor position
(customize-set-variable 'save-place-file (concat DIR_CACHE "places"))
(save-place-mode t)

(customize-set-variable 'require-final-newline t)

(customize-set-variable 'vc-follow-symlinks t)

(fset 'yes-or-no-p 'y-or-n-p)


(provide 'my-defaults)

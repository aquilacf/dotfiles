
(use-package doom-themes
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-monokai-pro t)
  (doom-themes-org-config))

(use-package doom-modeline
  :demand t
  :custom
  (doom-modeline-height 30)
  (mode-line-percent-position nil)
  :config (doom-modeline-mode t))

(custom-set-variables
 '(visible-bell nil)
 '(ring-bell-function 'ignore)
 '(use-dialog-box nil))

;; Line numbers
(customize-set-variable 'display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(column-number-mode)
(customize-set-variable 'truncate-lines t)

(use-package all-the-icons :if (display-graphic-p))
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init  (all-the-icons-completion-mode))


(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

(custom-set-variables
 '(show-paren-delay 0)
 '(show-paren-style 'parenthesis))

(set-face-attribute 'show-paren-match nil
  :foreground 'unspecified
  :background 'unspecified
  :weight 'semi-bold
  :inherit 'region)

(show-paren-mode)


(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'stack)
  (highlight-indent-guides-delay 0)
  :hook (prog-mode . highlight-indent-guides-mode))

(defun acf/infer-indentation-style ()
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> tab-count space-count)
      (customize-set-variable 'indent-tabs-mode t)
      (customize-set-variable 'indent-tabs-mode nil))))

(add-hook 'prog-mode-hook 'acf/infer-indentation-style)
(add-to-list 'write-file-functions 'delete-trailing-whitespace)

(use-package diff-hl
  :defer 2
  :hook
  (find-file    . diff-hl-mode)
  (vc-dir-mode  . diff-hl-dir-mode)
  (dired-mode   . diff-hl-dired-mode)
  (diff-hl-mode . diff-hl-margin-mode)
  (diff-hl-mode . diff-hl-flydiff-mode)
  (diff-hl-mode . global-diff-hl-show-hunk-mouse-mode)
  :custom
  (vc-git-diff-switches '("--histogram"))
  (diff-hl-show-staged-changes nil)
  (diff-hl-draw-borders nil)
  (diff-hl-margin-symbols-alist
   '((insert . "\u2502")
     (delete . "▸")
     (change . "\u2502")
     (unknown . "")
     (ignored . "")))
  :custom-face
  (diff-hl-insert ((t (:inherit default :background unspecified :foreground "green"))))
  (diff-hl-change ((t (:inherit default :background unspecified :foreground "yellow"))))
  (diff-hl-delete ((t (:inherit default :background unspecified :foreground "red"))))
  :config
  (global-diff-hl-mode))

(global-hl-line-mode)

(use-package minimap :custom (minimap-window-location 'right))

(use-package which-key
  :defer 5
  :custom (which-key-idle-delay 0.3)
  :config (which-key-mode t))

;;;;;;;;;;;;;
;; Editing;;
;;;;;;;;;;;;;
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

;; delete-word
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)
(delete-selection-mode)

;; Mouse wheel
(xterm-mouse-mode t)

(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

(global-unset-key (kbd "C-t")) ; This is reserved to tmux

(when IS_MAC
  (setq mac-right-option-modifier nil))

;; Make ESC quit prompts
(global-unset-key (kbd "C-x <escape> <escape>"))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(define-key isearch-mode-map (kbd "<DEL>") 'isearch-del-char)

(defun acf/save-all-buffers ()
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'acf/save-all-buffers)

(provide 'acf-ui)
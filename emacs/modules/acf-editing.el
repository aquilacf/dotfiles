;;; acf-editing.el --- Editing module.

;;; Commentary:
;; This module contains text editing related settings.

;; -*- lexical-binding: t -*-

;;; Code:

(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

(ffap-bindings)

;;TODO
;;(windmove-default-keybindings 'meta)
;; (defun ignore-error-wrapper (fn)
;;   "Funtion return new function that ignore errors.
;;    The function wraps a function with `ignore-errors' macro."
;;   (lexical-let ((fn fn))
;;     (lambda ()
;;       (interactive)
;;       (ignore-errors
;;         (funcall fn)))))

;; (global-set-key [M-left] (ignore-error-wrapper 'windmove-left))
;; (global-set-key [M-right] (ignore-error-wrapper 'windmove-right))
;; (global-set-key [M-up] (ignore-error-wrapper 'windmove-up))
;; (global-set-key [M-down] (ignore-error-wrapper 'windmove-down))


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

(when IS_MAC
  (setq mac-right-option-modifier nil))

;; Make ESC quit prompts
(global-unset-key (kbd "C-x <escape> <escape>"))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(define-key isearch-mode-map (kbd "<DEL>") 'isearch-del-char)
(define-key isearch-mode-map (kbd "C-<backspace>") 'isearch-del-char)
(define-key isearch-mode-map (kbd "<M-DEL>") 'isearch-del-char)

(defun acf/save-all-buffers ()
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'acf/save-all-buffers)

(electric-pair-mode +1)

;;TODO Conflicts with copilot
(use-package undo-tree
  :disabled
  :defer 2
  ;; :bind (:map undo-tree-mode
  ;;             ("<ret>" . undo-tree-undo)
  ;;             ("<S-ret>" . undo-tree-redo)
  ;;             ("C-/" . undo-tree-visualize))
  :custom (undo-tree-auto-save-history nil)
  :config (global-undo-tree-mode +1))
(use-package evil :disabled)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(provide 'acf-editing)

;;; acf-editing.el ends here

;; -*- lexical-binding: t -*-

(setq backup-inhibited t)
(setq make-backup-files nil)

(setq auto-save-default nil)
(setq auto-save-interval 0)
(setq auto-save-timeout 0)
(setq auto-save-list-file-prefix nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieve PATH from zshrc ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/update-path-from-zshell()
  "Update exec-path and PATH with zshrc PATHs."
  (let ((path (shell-command-to-string "TERM=dumb . ~/.zshrc; echo -n $PATH")))
    (setenv "PATH" path)
    (setq exec-path (split-string-and-unquote path path-separator))))

(when (display-graphic-p)
  (my/update-path-from-zshell))


;;;;;;;;;;;;;
;; Package ;;
;;;;;;;;;;;;;
(require 'seq)
(cond ((seq-empty-p package-archive-contents)
       (progn
	 (message "crafted-init: package archives empty, initializing")
	 (package-refresh-contents)))
      ((crafted-package-archives-stale-p)
       (progn
	 (message "crafted-init: package archives stale, refreshing in the background")
	 (package-refresh-contents t))))

(customize-set-variable 'package-enable-at-startup t)


;;;;;;;;
;; UI ;;
;;;;;;;;
(require 'crafted-ui)

(crafted-package-install-package 'doom-themes)

(custom-set-variables '(crafted-ui-display-line-numbers t)
		      '(doom-modeline-height 25))

(crafted-package-install-package 'rainbow-mode)
(rainbow-mode 1)

(custom-set-variables '(visible-bell nil)
		      '(ring-bell-function 'ignore)
		      '(use-dialog-box nil))

;;;;;;;;;;;;;;
;; Defaults ;;
;;;;;;;;;;;;;;
(require 'crafted-defaults)


;(defalias 'ctl-spc-keymap (make-sparse-keymap))
;(defvar ctl-spc-map (symbol-function 'ctl-spc-keymap))
;(define-key global-map (kbd "C-SPC") 'ctl-spc-keymap)

;;;;;;;;;;;;;
;; Editing ;;
;;;;;;;;;;;;;
(require 'crafted-editing)

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

(when ON-MAC (customize-set-variable 'mac-right-option-modifier nil)) ; Make right option behave like AltGr.

(customize-set-variable 'crafted-editing-prefer-tabs t)

(crafted-package-install-package 'editorconfig)
(editorconfig-mode 1)


;;;;;;;;;;;;;;
;; Projects ;;
;;;;;;;;;;;;;;
(require 'crafted-project)

;; Source Control
(crafted-package-install-package 'magit)
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

(global-set-key (kbd "C-M-;") #'magit-status)

(crafted-package-install-package 'git-gutter)
(custom-set-variables '(git-gutter:added-sign " +")
		      '(git-gutter:deleted-sign " -")
		      '(git-gutter:modified-sign " \u2502")
		      '(git-gutter:separator-sign nil)
		      '(git-gutter:unchanged-sign nil)
		      '(git-gutter:lighter nil)
		      '(git-gutter:window-width 2)
		      '(git-gutter:visual-line t)
		      '(git-gutter:update-interval 0))

(custom-set-faces '(git-gutter:added nil :inherit 'default :foreground "green1" :background 'unspecified)
		  '(git-gutter:deleted nil :inherit 'default :foreground "red" :background 'unspecified)
		  '(git-gutter:modified nil :inherit 'default :foreground "yellow" :background 'unspecified)
		  '(git-gutter:unchanged nil :inherit 'default :foreground 'unspecified :background 'unspecified))
(global-git-gutter-mode)


;;;;;;;;;
;; ORG ;;
;;;;;;;;;
;;(require 'crafted-org)


;;;;;;;;;;;;;;;;
;; Completion ;;
;;;;;;;;;;;;;;;;
(require 'crafted-completion)

(corfu-history-mode 1)
(add-to-list 'savehist-additional-variables 'corfu-history)

;;;;;;;;;
;; IDE ;;
;;;;;;;;;
(require 'crafted-ide)

(add-hook 'conf-toml-mode-hook 'eglot-ensure)

(crafted-package-install-package 'typescript-mode)
(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'js-mode-hook 'eglot-ensure)

(crafted-package-install-package 'graphql-mode)
;(add-to-list 'eglot-server-programs '(graphql-mode . ("graphql-lsp" "server" "--method=stream")))
;(add-hook 'graphql-mode-hook 'eglot-ensure)

(crafted-package-install-package 'json-mode)
(add-hook 'json-mode-hook 'eglot-ensure)

(crafted-package-install-package 'yaml-mode)
(add-hook 'yaml-mode-hook 'eglot-ensure)

(crafted-package-install-package 'markdown-mode)
;(add-to-list 'eglot-server-programs '(markdown-mode . ("remark-language-server"))) ;todo
(add-hook 'markdown-mode-hook 'eglot-ensure)

(crafted-package-install-package 'cmake-mode)
(add-hook 'cmake-mode-hook 'eglot-ensure)

(crafted-package-install-package 'terraform-mode)
(add-hook 'terraform-mode 'eglot-ensure)
(with-eval-after-load "eglot"
  (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve"))))





(crafted-package-install-package 'powershell)

(crafted-package-install-package 'tree-sitter)
(crafted-package-install-package 'tree-sitter-langs)
(crafted-package-install-package 'tree-sitter-indent)
(crafted-package-install-package 'csharp-mode)
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode))
(add-hook 'csharp-mode-hook 'eglot-ensure)
(with-eval-after-load "eglot"
  (add-to-list 'eglot-server-programs '(csharp-tree-sitter-mode . ("csharp-ls"))))





(crafted-package-install-package 'plantuml-mode)

(crafted-package-install-package 'docker)
(crafted-package-install-package 'dockerfile-mode)

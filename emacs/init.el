;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;
;; Defaults ;;
;;;;;;;;;;;;;;
(setq read-process-output-max 1048576) ;; 1mb
(defun my/update-path-from-zshell()
  "Update exec-path and PATH with zshrc PATHs."
  (let ((path (shell-command-to-string "TERM=dumb . ~/.zshrc; echo -n $PATH")))
    (setenv "PATH" path)
    (setq exec-path (split-string-and-unquote path path-separator))))

(if IS_MAC (my/update-path-from-zshell))

(use-package no-littering
  :demand t
  :config (customize-set-variable 'auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

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

;;;;;;;;
;; UI ;;
;;;;;;;;
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


(use-package all-the-icons :if (display-graphic-p) :defer 0)
(use-package all-the-icons-completion
  :after (all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init  (all-the-icons-completion-mode))

(use-package rainbow-mode
  :defer 3
  :config (rainbow-turn-on))

;;;;;;;;;;;;;;;;
;; Completion ;;
;;;;;;;;;;;;;;;;
(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-reverse
				vertico-directory))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  :init
  (vertico-mode)
  (vertico-reverse-mode)

  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
		(setq cand (funcall orig cand prefix suffix index _start))
		(concat
		 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
		 cand))))

(use-package marginalia
  :custom (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init  (marginalia-mode))


(use-package consult
  :demand t
  :custom (completion-in-region-function #'consult-completion-in-region)
  (consult-narrow-key ">")
  :bind ("C-x b" . consult-buffer)
  :config
  (consult-customize consult-grep consult-find :preview-key '(:debounce 0.2 any))
  (define-key minibuffer-local-map (kbd "C-r") 'consult-history)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (advice-add #'project-find-regexp :override #'consult-grep)
  (advice-add #'project-switch-to-buffer :override #'consult-project-buffer))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '(file (styles . (partial-completion)))))

(use-package corfu
  :defer 2
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-history))
  :bind (:map corfu-map ("SPC" . corfu-insert-separator))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 3)
  (corfu-auto-delay 0.0)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  (corfu-preview-current 'insert)
  (corfu-preselect-first nil)
  (corfu-on-exact-match nil)
  (corfu-min-width 80)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-popupinfo-delay t)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  (add-to-list 'savehist-additional-variables 'corfu-history)
  (unbind-key "M-SPC" corfu-map))

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :defer 2
  :straight (corfu-terminal
	     :type git
	     :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")

  :config (corfu-terminal-mode))

(use-package kind-icon :after corfu :init (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;;;;;;;;;;;;;
;; Projects ;;
;;;;;;;;;;;;;;
(use-package git-gutter
  :defer 2
  :custom
  (git-gutter:added-sign " +")
  (git-gutter:deleted-sign " -")
  (git-gutter:modified-sign " \u2502")
  (git-gutter:separator-sign nil)
  (git-gutter:unchanged-sign nil)
  (git-gutter:lighter nil)
  (git-gutter:window-width 2)
  (git-gutter:update-interval 0)

  :config
  (set-face-attribute 'git-gutter:added nil :inherit 'default :foreground "green1" :background 'unspecified)
  (set-face-attribute 'git-gutter:deleted nil :inherit 'default :foreground "red" :background 'unspecified)
  (set-face-attribute 'git-gutter:modified nil :inherit 'default :foreground "yellow" :background 'unspecified)
  (set-face-attribute 'git-gutter:unchanged nil :inherit 'default :foreground 'unspecified :background 'unspecified)
  (global-git-gutter-mode))

;; Dired
(customize-set-variable 'dired-kill-when-opening-new-dired-buffer t)
(customize-set-variable 'dired-use-ls-dired nil)
(with-eval-after-load 'dired
  (define-key dired-mode-map [mouse-2] 'dired-find-file))

;; (use-package dirvish
;;   :custom ((dirvish-reuse-session nil)
;;            (dirvish-attributes '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
;;            (dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")
;;            (dired-mouse-drag-files t)
;;            (insert-directory-program "gls")
;;            (mouse-drag-and-drop-region-cross-program t))
;;   :init
;;   (dirvish-override-dired-mode))


;;;;;;;;;
;; IDE ;;
;;;;;;;;;
(use-package editorconfig :demand t :config (editorconfig-mode))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;;;;;;;;;
;; LSP ;;
;;;;;;;;;
(use-package eglot
  :disabled
  :ensure-system-package
  ((bash-language-server . "yarn global add bash-language-server")
   (taplo . "cargo install taplo-cli")
   (tsc . "yarn global add typescript")
   (typescript-language-server . "yarn global add typescript-language-server")
   (yaml-language-server . "yarn global add yaml-language-server")
   (cmake-language-server . "pip3 install cmake-language-server"))
  :bind (("M-<RET>" . eglot-code-actions)
         ("s-r r" . eglot-rename)
         ("M-s-<return>" . eglot-format))
  :hook
  ((sh-mode . eglot-ensure)
   (conf-toml-mode . eglot-ensure)
   (c++-mode . eglot-ensure)
   (c-mode . eglot-ensure)
   (mhtml-mode . eglot-ensure)
   (css-mode . eglot-ensure)
   (terraform-mode . eglot-ensure)
   (yaml-ts-mode . eglot-ensure)
   (csharp-ts-mode . eglot-ensure)
   (csharp-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)
   (json-ts-mode . eglot-ensure)
   (cmake-ts-mode . eglot-ensure))
  :custom (eglot-autoshutdown t))

(with-eval-after-load 'eglot
  (push '(conf-toml-mode . ("taplo" "lsp" "stdio")) eglot-server-programs)
  (push '(terraform-mode . ("terraform-ls" "serve")) eglot-server-programs)
  (push '(graphql-mode . ("graphql-lsp" "server" "-m" "stream")) eglot-server-programs)
  (push '(csharp-ts-mode . ("/bin/ksh" "-c" "csharp-ls")) eglot-server-programs)
  (push '(csharp-mode . ("/bin/ksh" "-c" "csharp-ls")) eglot-server-programs)
  (push '(json-ts-mode . ("vscode-json-language-server" "--stdio")) eglot-server-programs)
  (push '(cmake-ts-mode . ("cmake-language-serve")) eglot-server-programs)
  (push '(yaml-ts-mode . ("yaml-language-server" "--stdio")) eglot-server-programs))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :bind (
         ("M-<RET>" . lsp-execute-code-action)
         ("s-r r" . lsp-rename)
         ("M-s-<return>" . lsp-format-buffer))
  :custom
  (lsp-idle-delay 0)
  (lsp-enable-snippet nil)
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-provider :none)
  (lsp-diagnostics-provider :flymake)
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook
  ((lsp-completion-mode . my/lsp-mode-setup-completion)
   (lsp-mode . lsp-enable-which-key-integration)
   (sh-mode . lsp-deferred)
   (conf-toml-mode . lsp-deferred)
   (c++-mode . lsp-deferred)
   (c-mode . lsp-deferred)
   (mhtml-mode . lsp-deferred)
   (css-mode . lsp-deferred)
   (csharp-mode . lsp-deferred)
   (terraform-mode . lsp-deferred)
   (js-json-mode . lsp-deferred)
   (yaml-mode . lsp-deferred)
   (typescript-mode . lsp-deferred)
   (cmake-mode . lsp-deferred)))

;; (use-package dap-mode
;;   :hook csharp-mode
;;   :custom (dap-netcore-install-dir (concat no-littering-var-directory "dap-cshap"))
;;   :config
;;   (require 'dap-netcore))

;;;;;;;;
;; C# ;;
;;;;;;;;
(use-package csproj-mode)
(use-package powershell)


;;;;;;;;;;;;;;
;; Markdown ;;
;;;;;;;;;;;;;;
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode ("README\\.md\\'" . gfm-mode))

(defvar markdown-electric-pairs '((?* . ?*)) "Electric pairs for markdown-mode.")
(defun markdown-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs markdown-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(add-hook 'markdown-ts-mode-hook 'markdown-add-electric-pairs)

;;;;;;;;;;;;
;; Docker ;;
;;;;;;;;;;;;
(use-package docker :bind ("C-c d" . docker))

;;;;;;;;;;;;;;;
;; Terraform ;;
(use-package terraform-mode
  :mode "\\.tf\\'"
  :ensure-system-package
  ((terraform)
   (terraform-ls)))

;;;;;;;;;;;;;;;;
;; Treesitter ;;
;;;;;;;;;;;;;;;;
(use-package treesit-auto
  :disabled
  :custom (treesit-auto-install 'prompt)
  :init (global-treesit-auto-mode))

;;;;;;;;;;;;;
;; GraphQL ;;
;;;;;;;;;;;;;
(use-package graphql-mode
  :mode "\\.graphql\\'"
  :ensure-system-package
  (graphql-lsp . "yarn global add graphql graphql-language-service-cli"))

;;;;;;;;;;;;;;
;; PlantUML ;;
;;;;;;;;;;;;;;
(use-package plantuml-mode
  :mode "\\.p[lant]?uml\\'"
  :ensure-system-package (plantuml . plantuml)
  :custom
  (plantuml-executable-path "plantuml")
  (plantuml-default-exec-mode 'executable))

;;;;;;;;;
;; PDF ;;
;;;;;;;;;
(use-package pdf-tools :hook (doc-view-mode . (lambda () (require 'pdf-tools))))

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
;; (global-set-key (kbd "<mouse-3>") (kbd "C-y"))
;; (global-set-key (kbd "<mouse-4>") (kbd "C-u 1 M-v"))
;; (global-set-key (kbd "<mouse-5>") (kbd "C-u 1 C-v"))

(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

(global-unset-key (kbd "C-t")) ; This is reserved to tmux

(when IS_MAC
  (setq mac-right-option-modifier nil))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(define-key isearch-mode-map (kbd "<DEL>") 'isearch-del-char)

(defun my/save-all-buffers ()
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'my/save-all-buffers)

(use-package minimap :custom (minimap-window-location 'right))

(use-package which-key
  :defer 5
  :custom (which-key-idle-delay 0.3)
  :config (which-key-mode t))

;(defalias 'ctl-spc-keymap (make-sparse-keymap))
;(defvar ctl-spc-map (symbol-function 'ctl-spc-keymap))
;(define-key global-map (kbd "C-SPC") 'ctl-spc-keymap)

;(use-package hydra :disabled)

;; (defhydra hydra-text-scale (:timeout 4)
;;   "scale text"
;;   ("j" text-scale-increase "in")
;;   ("k" text-scale-decrease "out")
;;   ("f" nil "finished" :exit t))

;; (my/leader-keys
;;  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package comment-dwim-2
  :defer 2
  :bind (("M-s-/" . comment-dwim-2)
	 :map org-mode-map
	 ("M-s-/" . org-comment-dwim-2)))

(electric-pair-mode)

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
  (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-responsive 'stack)
  (highlight-indent-guides-delay 0)
  :hook (prog-mode . highlight-indent-guides-mode))

(defun my/infer-indentation-style ()
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> tab-count space-count)
      (customize-set-variable 'indent-tabs-mode t)
      (customize-set-variable 'indent-tabs-mode nil))))

(add-hook 'prog-mode-hook 'my/infer-indentation-style)
(add-to-list 'write-file-functions 'delete-trailing-whitespace)

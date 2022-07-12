;; -*- lexical-binding: t -*-
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

;;;;;;;;;;;;;;;;;;
;; Key Bindings ;;
;;;;;;;;;;;;;;;;;;
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

;; Mouse wheel
(xterm-mouse-mode t)
(global-set-key (kbd "<mouse-3>") (kbd "C-y"))
(global-set-key (kbd "<mouse-4>") (kbd "C-u 1 M-v"))
(global-set-key (kbd "<mouse-5>") (kbd "C-u 1 C-v"))

(global-unset-key (kbd "C-t")) ; This is reserved to tmux

(when IS_MAC
  (setq mac-right-option-modifier nil))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; Evil
(use-package evil
  :disabled
  :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
;  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode t)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  ;; (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  ;; (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

;; (use-package counsel
;;   :bind (("C-M-j" . 'counsel-switch-buffer)
;;          :map minibuffer-local-map
;;          ("C-r" . 'counsel-minibuffer-history))
;;   :custom
;;   (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
;;   :config
;;   (counsel-mode 1))

(use-package which-key
  :defer 5
  :custom (which-key-idle-delay 0.3)
  :config (which-key-mode t))

(use-package ivy
  ;; :bind (("C-s" . swiper)
  ;;        :map ivy-minibuffer-map
  ;;        ("TAB" . ivy-alt-done)
  ;;        ("C-l" . ivy-alt-done)
  ;;        ("C-j" . ivy-next-line)
  ;;        ("C-k" . ivy-previous-line)
  ;;        :map ivy-switch-buffer-map
  ;;        ("C-k" . ivy-previous-line)
  ;;        ("C-l" . ivy-done)
  ;;        ("C-d" . ivy-switch-buffer-kill)
  ;;        :map ivy-reverse-i-search-map
  ;;        ("C-k" . ivy-previous-line)
  ;;        ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; (use-package ivy-rich
;;   :after ivy
;;   :init
;;   (ivy-rich-mode 1))


(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
 ; :custom
 ; (counsel-describe-function-function #'helpful-callable)
 ; (counsel-describe-variable-function #'helpful-variable)
  :bind
;  ([remap describe-function] . counsel-describe-function)
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
;  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))


(use-package general
  :disabled
  :config
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    ;:prefix "SPC"
    :global-prefix "C-SPC")

  (my/leader-keys
    "t"  '(:ignore t :which-key "toggles")))
    ;"tt" '(counsel-load-theme :which-key "choose theme")))

(use-package hydra :disabled)

;; (defhydra hydra-text-scale (:timeout 4)
;;   "scale text"
;;   ("j" text-scale-increase "in")
;;   ("k" text-scale-decrease "out")
;;   ("f" nil "finished" :exit t))

;; (my/leader-keys
;;  "ts" '(hydra-text-scale/body :which-key "scale text"))


;;;;;;;;;;;;;;;;;
;; Minor Modes ;;
;;;;;;;;;;;;;;;;;

(use-package editorconfig
  :demand t
  :config (editorconfig-mode t))
(use-package smartparens
  :defer 2
  :config (smartparens-global-mode t))
(use-package rainbow-mode
  :defer 5
  :config (rainbow-mode))


;;;;;;;;;;;;;;
;; Projects ;;
;;;;;;;;;;;;;;
(use-package projectile
  :demand t
  :custom
  (projectile-known-projects-file (concat DIR_CACHE "projectile-bookmarks.eld"))
  (projectile-cache-file (concat DIR_CACHE "projectile.cache"))
  (projectile-switch-project-action #'projectile-commander)
  (projectile-project-search-path '(("~/Projects" . 3)))
  ;(projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode))


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


;;;;;;;;;;;;;
;; Company ;;
;;;;;;;;;;;;;
(use-package company
  :disabled
  :custom
  (company-idle-delay 0.5)
  (company-echo-delay 0)
  (company-show-numbers t)
  (company-selection-wrap-around t)
  (company-minimum-prefix-length 3)
  (company-tooltip-align-annotations t)
  (company-dabbrev-downcase nil)
  :config (global-company-mode))


(use-package company-statistics
  :after (company)
  :custom (company-statistics-file (concat DIR_CACHE "company-statistics-cache.el"))
  :config (company-statistics-mode))

(use-package company-box :hook (company-mode . company-box-mode))

(use-package company-quickhelp
  :after (company)
  :hook (company-mode . company-quickhelp-mode)
  :custom (company-quickhelp-delay 0))

(use-package company-quickhelp-terminal
  :after (company-quickhelp)
  :hook (company-quickhelp-mode . company-quickhelp-terminal-mode))


;;;;;;;;;;;;;
;; Flycheck ;;
;;;;;;;;;;;;;
;; (use-package flycheck
;; 	:defer 2
;; 	:config (global-flycheck-mode))


;;;;;;;;;;;;;;;;
;; Completion ;;
;;;;;;;;;;;;;;;;
(use-package marginalia
  :custom (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '(file (styles . (partial-completion)))))

  
(use-package corfu
  :custom
   (corfu-cycle t)
   (corfu-auto t)
   (corfu-auto-prefix 2)
   (corfu-auto-delay 0.0)
   (corfu-echo-documentation 0.25)
  :init
  (global-corfu-mode))

(use-package corfu-doc
  :after corfu
  :hook (corfu-mode . corfu-doc-mode))


(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Terminal
(use-package popon
  :straight (:type git :host nil :repo "https://codeberg.org/akib/emacs-popon.git")
  :if (not (display-graphic-p)))

(use-package corfu-terminal
  :straight (:type git :host nil :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :after popon
  :init
  (global-corfu-terminal-mode))

(use-package corfu-doc-terminal
  :straight (corfu-doc-terminal :type git :host nil :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")
  :after corfu-terminal
  :hook (corfu-mode-terminal . corfu-doc-terminal-mode))

;;;;;;;;;
;; LSP ;;
;;;;;;;;;
(defconst DIR_LSP (concat DIR_CACHE "lsp"))
(defconst DIR_DAP (concat DIR_CACHE "dap"))
(use-package lsp-mode
  :ensure-system-package
  ((bash-language-server . "yarn global add bash-language-server")
   (taplo . "cargo install taplo-cli"))
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (sh-mode . lsp-deferred)
   (conf-mode . lsp-deferred))
  :custom
  (lsp-server-install-dir DIR_LSP)
  (lsp-session-file (concat DIR_CACHE "lsp-session"))
  (lsp-keymap-prefix "C-c l")
  (lsp-auto-guess-root t)
  (lsp-enable-suggest-server-download nil)
  :commands (lsp lsp-deferred))


;(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package dap-mode
  :custom
  (dap-breakpoints-file (concat DIR_CACHE "dap-breakpoints"))
  :commands dap-debug
;  :config
  ;; (general-define-key
  ;;   :keymaps 'lsp-mode-map
  ;;   :prefix lsp-keymap-prefix
  ;;   "d" '(dap-hydra t :wk "debugger"))
  )

;;;;;;;;;;;;;;;;;
;; Major Modes ;;
;;;;;;;;;;;;;;;;;

;;;;;;;;
;; C# ;;
;;;;;;;;
(use-package tree-sitter)
(use-package tree-sitter-langs)
(use-package tree-sitter-indent)

(defun my/csharp-mode-hook ()
  (lsp-deferred)
  (electric-pair-mode 1))

(use-package csharp-mode
  :mode "\\.cs\\'"
  :hook (csharp-tree-sitter-mode . my/csharp-mode-hook) ; check this later
  :init (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode))
  :custom
  (dap-netcore-install-dir DIR_DAP)
  :config
  (require 'dap-netcore)
  (dap-netcore-setup))


(use-package powershell)


;;;;;;;;;;;;;;
;; Markdown ;;
;;;;;;;;;;;;;;
(use-package markdown-mode
  :mode "\\.md\\'"
  :ensure-system-package (remark-language-server . "yarn global add remark-language-server")
  :hook (markdown-mode . lsp-deferred))


;;;;;;;;;;;;
;; Docker ;;
;;;;;;;;;;;;
(use-package docker :bind ("C-c d" . docker))
(use-package dockerfile-mode :mode "Dockerfile\\'")


;;;;;;;;;;;;;;;
;; Terraform ;;
;;;;;;;;;;;;;;;
(use-package terraform-mode
  :mode "\\.tf\\'"
  :ensure-system-package
  ((terraform)
   (terraform-ls))
  :hook (terraform-mode . lsp-deferred))


;;;;;;;;;;;;;;;;
;; TypeScript ;;
;;;;;;;;;;;;;;;;
(use-package typescript-mode
  :mode "\\.ts[x]?\\'"
  :ensure-system-package
  ((tsc . "yarn global add typescript")
   (typescript-language-server . "yarn global add typescript-language-server"))
  :hook (typescript-mode . lsp-deferred))

;; JS
(use-package js2-mode
  :mode "\\.js[x]?\\'"
  :hook (js2-mode . lsp-deferred))

;;;;;;;;;;
;; YAML ;;
;;;;;;;;;;
(use-package yaml-mode
  :mode "\\.yml\\'"
  :ensure-system-package (yaml-language-server . "yarn global add yaml-language-server")
  :custom (lsp-yaml-schemas t)  
  :hook (yaml-mode . lsp-deferred))


;;;;;;;;;;
;; JSON ;;
;;;;;;;;;;
(use-package json-mode
  :ensure-system-package (vscode-json-language-server . "yarn global add vscode-langservers-extracted")
  :custom (lsp-json-schemas t)
  :hook (json-mode . lsp-deferred))


;;;;;;;;;;;;;
;; GraphQL ;;
;;;;;;;;;;;;;
(use-package graphql-mode
  :mode "\\.graphql\\'"
  :ensure-system-package
  (graphql-lsp . "yarn global add graphql graphql-language-service-cli")
  :init
  ;; Not working
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration
		 '(graphql-mode . "graphql"))

    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection '("graphql-lsp" "server" "--method=stream"))
                      :major-modes '(graphql-mode)
                      :language-id "graphql"
                      :server-id 'graphql-lsp
                      :priority 1
                      :add-on? t
                      :multi-root t
                      :activation-fn (lsp-activate-on "graphql")))))



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
;; Org ;;
;;;;;;;;;
(use-package org)
(setq org-element-cache-persistent nil)
(use-package org-bullets :hook (org-mode . org-bullets-mode))


;;;;;;;;;;;
;; C/C++ ;;
;;;;;;;;;;;
(use-package cmake-mode
  :ensure-system-package (cmake-language-server . "pip3 install cmake-language-server")
  :hook (cmake-mode . lsp-deferred))


;;;;;;;;;;;;
;; Gutter ;;
;;;;;;;;;;;;
(use-package git-gutter
  :defer 1
  :custom
  (git-gutter:added-sign " +")
  (git-gutter:deleted-sign " -")
  (git-gutter:modified-sign " \u2502")
  (git-gutter:separator-sign nil)
  (git-gutter:unchanged-sign nil)
  (git-gutter:lighter nil)
  (git-gutter:window-width 2)
  (git-gutter:visual-line t)
  (git-gutter:update-interval 0)

  :config
  (set-face-attribute 'git-gutter:added nil :inherit 'default :foreground "green1" :background 'unspecified)
  (set-face-attribute 'git-gutter:deleted nil :inherit 'default :foreground "red" :background 'unspecified)
  (set-face-attribute 'git-gutter:modified nil :inherit 'default :foreground "yellow" :background 'unspecified)
  (set-face-attribute 'git-gutter:unchanged nil :inherit 'default :foreground 'unspecified :background 'unspecified)
  (global-git-gutter-mode))


;;;;;;;;;;;
;; Magit ;;
;;;;;;;;;;;
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (transient-history-file (concat DIR_CACHE "transient/history.el"))
  (transient-values-file (concat DIR_CACHE "transient/values.el"))
  (transient-levels-file (concat DIR_CACHE "transient/levels.el")))


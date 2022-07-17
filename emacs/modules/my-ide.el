;; -*- lexical-binding: t -*-

(use-package editorconfig
  :demand t
  :config (editorconfig-mode t))


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
  (lsp-toml-cache-path (concat DIR_LSP "/lsp-toml"))
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


(provide 'my-ide)

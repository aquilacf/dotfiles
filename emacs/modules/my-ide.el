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
(use-package eglot
  :ensure-system-package
  ((bash-language-server . "yarn global add bash-language-server")
   (taplo . "cargo install taplo-cli"))
  :hook
  ((sh-mode . eglot-ensure)
   (conf-toml-mode . eglot-ensure)
   (c++-mode . eglot-ensure)
   (c-mode . eglot-ensure)))

(with-eval-after-load 'eglot
  (push '(conf-toml-mode . ("taplo" "lsp" "stdio")) eglot-server-programs)
  (push '(terraform-mode . ("terraform-ls" "serve")) eglot-server-programs)
  (push '(graphql-mode . ("graphql-lsp" "server" "-m" "stream")) eglot-server-programs)
  (push '(csharp-tree-sitter-mode . ("/bin/ksh" "-c" "csharp-ls")) eglot-server-programs)
  (define-key eglot-mode-map (kbd "s-r r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "M-s-<return>") 'eglot-format))

;;;;;;;;
;; C# ;;
;;;;;;;;
(use-package tree-sitter)
(use-package tree-sitter-langs)
(use-package tree-sitter-indent)

(defun my/csharp-mode-hook ()
  (eglot-ensure)
  (electric-pair-mode 1))

(use-package csharp-mode
  :mode "\\.cs\\'"
  :hook (csharp-tree-sitter-mode . my/csharp-mode-hook) ; check this later
  :init (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode)))

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

(add-hook 'markdown-mode-hook 'markdown-add-electric-pairs)

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
  :hook (terraform-mode . eglot-ensure))


;;;;;;;;;;;;;;;;
;; TypeScript ;;
;;;;;;;;;;;;;;;;
(use-package typescript-mode
  :mode "\\.ts[x]?\\'"
  :ensure-system-package
  ((tsc . "yarn global add typescript")
   (typescript-language-server . "yarn global add typescript-language-server"))
  :hook (typescript-mode . eglot-ensure))

;; JS
(use-package js2-mode
  :mode "\\.js[x]?\\'"
  :hook (js2-mode . eglot-ensure))

;;;;;;;;;;
;; YAML ;;
;;;;;;;;;;
(use-package yaml-mode
  :mode "\\.yml\\'"
  :ensure-system-package (yaml-language-server . "yarn global add yaml-language-server")
  :hook (yaml-mode . eglot-ensure))


;;;;;;;;;;
;; JSON ;;
;;;;;;;;;;
(use-package json-mode
  :ensure-system-package (vscode-json-language-server . "yarn global add vscode-langservers-extracted")
  :hook (json-mode . eglot-ensure))


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
  :hook (cmake-mode . eglot-ensure))


(provide 'my-ide)

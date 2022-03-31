;;;;;;;;;;;;;;
;; Straight ;;
;;;;;;;;;;;;;;
(setq straight-base-dir DIR_CACHE)
(unless (featurep 'straight)
	;; Bootstrap straight.el
	(defvar bootstrap-version)
	(let ((bootstrap-file
		(expand-file-name "straight/repos/straight.el/bootstrap.el" DIR_CACHE))
		(bootstrap-version 5))
		(unless (file-exists-p bootstrap-file)
			(with-current-buffer
				(url-retrieve-synchronously "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el" 'silent 'inhibit-cookies)
				(goto-char (point-max))
				(eval-print-last-sexp)))
		(load bootstrap-file nil 'nomessage)))

;; Use Package
(straight-use-package 'use-package)
(straight-use-package 'use-package-ensure-system-package)
(setq straight-use-package-by-default t)
(setq use-package-expand-minimally t)


;;;;;;;;;;;
;; Theme ;;
;;;;;;;;;;;
(use-package doom-themes
	:custom
		(doom-themes-enable-bold t)
		(doom-themes-enable-italic t)
	:config
		(load-theme 'doom-one t)
		(doom-themes-org-config))


(defvar current-theme-style "d")
(defun toggle-theme ()
	"Toggle theme style."
	(interactive)
	(if (string-equal current-theme-style "d")
		(progn
			(disable-theme 'doom-one) (load-theme 'doom-one-light t) (setq current-theme-style "l"))
		(progn
			(disable-theme 'doom-one-light) (load-theme 'doom-one t) (setq current-theme-style "d"))))


;;;;;;;;;;;;;;;;;
;; Minor Modes ;;
;;;;;;;;;;;;;;;;;
(use-package which-key
	 :defer 2
	 :config (which-key-mode)
	 :custom (which-key-idle-delay 0.3))


(use-package rainbow-mode
	 :defer 2
	 :config (rainbow-mode))


(use-package editorconfig
	 :defer 2
	 :config (editorconfig-mode))


(use-package smartparens
	 :defer 2
	 :config (smartparens-global-mode))

(use-package comment-dwim-2
	:defer 2
	:bind
	(
		:map org-mode-map ("M-;" . org-comment-dwim-2)
		:map prog-mode-map ("M-;" . comment-dwim-2)
	))

;;;;;;;;;;;;
;; Gutter ;;
;;;;;;;;;;;;
(use-package git-gutter
	:defer 2
	:custom
	 (
	  (git-gutter:added-sign " +")
	  (git-gutter:deleted-sign " -")
	  (git-gutter:modified-sign " \u2502")
	  (git-gutter:separator-sign nil)
	  (git-gutter:unchanged-sign nil)
	  (git-gutter:lighter nil)
	  (git-gutter:window-width 2)
	  (git-gutter:visual-line t)
	  (git-gutter:update-interval 2)

	  )
	:config
	 (set-face-attribute 'git-gutter:added nil :inherit 'default :foreground "green1" :background 'unspecified)
	 (set-face-attribute 'git-gutter:deleted nil :inherit 'default :foreground "red" :background 'unspecified)
	 (set-face-attribute 'git-gutter:modified nil :inherit 'default :foreground "yellow" :background 'unspecified)
	 (set-face-attribute 'git-gutter:unchanged nil :inherit 'default :foreground 'unspecified :background 'unspecified)
	 (global-git-gutter-mode))

;;;;;;;;;;;;;;
;; Projects ;;
;;;;;;;;;;;;;;
(use-package projectile
	 :custom
	 (projectile-known-projects-file (concat DIR_CACHE "projectile-bookmarks.eld"))
	 :config
	 (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
	 (projectile-mode)
	 )

(projectile-register-project-type 'npm '("package.json")
	:compile "yarn install"
	:test "yarn test"
	:run "yarn start"
	:test-suffix ".spec" )

;;;;;;;;;;;;;
;; Company ;;
;;;;;;;;;;;;;
(use-package company
	:defer 2
	:custom
		(company-idle-delay 1)
		(company-echo-delay 0)
		(company-show-numbers t)
		(company-selection-wrap-around t)
		(company-minimum-prefix-length 3)
		(company-tooltip-align-annotations t)
		(company-dabbrev-downcase nil)
	:config (global-company-mode))


(use-package company-statistics
	:defer 2
	:after (company)
	:custom (company-statistics-file (concat DIR_CACHE "company-statistics-cache.el"))
	:config (company-statistics-mode))

(use-package company-box  :hook (company-mode . company-box-mode))

(use-package company-quickhelp
	:defer 2
	:after (company)
	:custom (company-quickhelp-delay 0)
	:hook (company-mode . company-quickhelp-mode))

(use-package company-quickhelp-terminal
	:defer 2
	:after (company-quickhelp)
	:hook (company-quickhelp-mode . company-quickhelp-terminal-mode))

;;;;;;;;;;;;;
;; Flycheck ;;
;;;;;;;;;;;;;
(use-package flycheck
	:defer 2
	:config (global-flycheck-mode))


;;;;;;;;;;;;;;;;;
;; Major Modes ;;
;;;;;;;;;;;;;;;;;


;;;;;;;;
;; C# ;;
;;;;;;;;
(use-package tree-sitter)
(use-package tree-sitter-langs)
(use-package tree-sitter-indent)

(defun my-csharp-mode-hook ()
 ;; enable the stuff you want for C# here
 (electric-pair-mode 1)       ;; Emacs 24
 (electric-pair-local-mode 1) ;; Emacs 25
 )
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

(use-package csharp-mode
	:after (tree-sitter)
	:hook (csharp-mode . my-csharp-mode-hook)
	:mode
		("\\.cs\\'" . csharp-tree-sitter-mode)
		("\\.cake\\'" . csharp-tree-sitter-mode)
	:ensure-system-package (dotnet . dotnet))

(use-package powershell)

;;;;;;;;;;;;;;
;; Markdown ;;
;;;;;;;;;;;;;;
(use-package markdown-mode)

;;;;;;;;;
;; PHP ;;
;;;;;;;;;
(use-package php-mode)

;;;;;;;;;;;;
;; Docker ;;
;;;;;;;;;;;;
(use-package docker :bind ("C-c d" . docker))
(use-package dockerfile-mode :mode "Dockerfile\\'")

;;;;;;;;;;;;;;;
;; Terraform ;;
;;;;;;;;;;;;;;;
(use-package terraform-mode
	:config (setq lsp-terraform-server "terraform-ls")
	:ensure-system-package (
		(terraform . terraform)
		(terraform-ls . "brew install hashicorp/tap/terraform-ls")))

(use-package company-terraform
	:after (terraform-mode company)
	:hook (terraform-mode . company-terraform-init))

;;;;;;;;;;;;;;;;
;; TypeScript ;;
;;;;;;;;;;;;;;;;
(use-package typescript-mode
	:mode ("\\.ts[x]?\\'" . typescript-mode)
	:ensure-system-package (
	  (typescript-language-server . "yarn global add typescript-language-server")
	  (tsc . "yarn global add typescript")))

;;;;;;;;;;
;; YAML ;;
;;;;;;;;;;
(use-package yaml-mode
	 :ensure-system-package (yaml-language-server . "yarn global add yaml-language-server")
	 :custom (lsp-yaml-schemas t))

(use-package toml-mode)

;;;;;;;;;;
;; JSON ;;
;;;;;;;;;;
;(use-package json-mode
;	 :ensure-system-package (vscode-json-languageserver . "yarn global add vscode-json-languageserver")
;	 :custom (lsp-json-schemas t))

;;;;;;;;;;;;;
;; GraphQL ;;
;;;;;;;;;;;;;
(use-package graphql-mode
	 :ensure-system-package (graphql-lsp . "yarn global add graphql-language-service-cli graphql"))

;;;;;;;;;;;;;;
;; PlantUML ;;
;;;;;;;;;;;;;;
(use-package plantuml-mode
	:config
	 (setq plantuml-executable-path "plantuml")
	 (setq plantuml-default-exec-mode 'executable)
	 :ensure-system-package (plantuml . plantuml)
	 :mode ("\\.p[lant]?uml\\'" . plantuml-mode))

;;;;;;;;;
;; Org ;;
;;;;;;;;;
(use-package org)
(use-package org-bullets :hook (org-mode . org-bullets-mode))


;; LSP
(use-package lsp-mode
	:defer 3
	:custom
		(lsp-session-file (concat DIR_CACHE "lsp-session"))
		(lsp-keymap-prefix "C-c l")
		(lsp-auto-guess-root t)
	:hook
	 (
	  (yaml-mode        . lsp-deferred)
	  (json-mode        . lsp-deferred)
	  (typescript-mode  . lsp-deferred)
	  (js-mode          . lsp-deferred)
	  (sh-mode          . lsp-deferred)
	  (graphql-mode     . lsp-deferred)
	  (terraform-mode   . lsp-deferred)
	  ;(csharp-tree-sitter-mode 	. lsp-deferred)
	  (lsp-mode . lsp-enable-which-key-integration)
	  )
	:config
	 (lsp-register-client
	  (make-lsp-client  :new-connection (lsp-stdio-connection '("graphql-lsp" "server" "--method" "stream"))
			    :major-modes '(graphql-mode)
			    :server-id 'graphql-lsp
			    )
	  )
	 (add-to-list 'lsp-language-id-configuration '(graphql-mode . "graphql"))
	:commands (lsp lsp-deferred)
	:ensure-system-package (bash-language-server . "yarn global add bash-language-server"))

;(use-package lsp-ui :commands lsp-ui-mode)
;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;(use-package dap-mode)
;(use-package dap-LANGUAGE) to load the dap adapter for your language

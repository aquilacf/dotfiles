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
	:after (company)
	:custom (company-statistics-file (concat DIR_CACHE "company-statistics-cache.el"))
	:config (company-statistics-mode))


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
	:config (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode))
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
	:ensure-system-package (
		(terraform . terraform)
		(terraform-ls . "brew install hashicorp/tap/terraform-ls")))

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


;;; packages.el --- Packages core.
;;; Commentary: My packages.
;;; Code:

(setq use-package-always-ensure t)
(use-package use-package-ensure-system-package)



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formatting and Visual ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-mode		:config (rainbow-mode))
(use-package which-key			:config (which-key-mode))
(use-package editorconfig		:config (editorconfig-mode))
(use-package smartparens		:config (smartparens-global-mode))

(use-package format-all
	:ensure-system-package
	(
		(yarn 		. yarn)
		(prettier 	. "yarn global add prettier @prettier/plugin-php prettier-plugin-pkg prettier-plugin-toml")
	)
)

; (use-package doom-modeline
; 	:ensure t
; 	:init (doom-modeline-mode)
; )

(use-package flycheck
	; :custom
	; 	(flycheck-highlighting-mode 'lines)
;	:config
		;(set-face-attribute 'flycheck-error nil :underline '(:color "red2" :style wave))
	;	(set-face-attribute 'flycheck-error nil :background "red" :foreground "white" :slant 'italic )
)

;;;;;;;;;;;;;;;;
;; Navigation ;;
;;;;;;;;;;;;;;;;
(use-package projectile
	:custom
		(projectile-known-projects-file (concat dir-cache "projectile-bookmarks.eld"))
	:config
		(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
		(projectile-mode)
)

(projectile-register-project-type 'npm '("package.json")
				  :compile "yarn install"
				  :test "yarn test"
				  :run "yarn start"
				  :test-suffix ".spec"
)

(use-package treemacs
	:custom
	(
		(treemacs-follow-mode t)
		(treemacs-filewatch-mode t)
		(treemacs-fringe-indicator-mode t)
	)
	:bind
		("M-0" . treemacs)
		; ("M-0"       . treemacs-select-window)
  ;       ("C-c t t"   . treemacs)
  ;       ("C-c t a"   . treemacs-add-and-display-current-project)
)


(use-package treemacs-projectile
	:after (treemacs projectile)
	:config
		(advice-add 'projectile-switch-project-by-name :after #'(lambda (&rest args)
																	(let* ((path (car args))
																		(name (treemacs--filename path)))
          															(treemacs-do-add-project-to-workspace path name))))
)


;;;;;;;;;;;;;;;;
;; Completion ;;
;;;;;;;;;;;;;;;;
(use-package yasnippet			:config (yas-global-mode))
(use-package yasnippet-snippets	:after (yasnippet))

(use-package company
	:custom
		(
			(company-idle-delay 1)
			(company-echo-delay 0)
			(company-show-numbers t)
			(company-selection-wrap-around t)
			(company-minimum-prefix-length 3)
			(company-tooltip-align-annotations t)
			(company-dabbrev-downcase nil)
			;(company-frontends '(company-box-frontend))
		)
	:bind
	(
		:map company-active-map
			("RET" . nil)
			("TAB" . company-complete-selection)
			("<tab>" . company-complete-selection)
			("C-s" . company-complete-selection)  ; Mostly to use during yasnippet expansion
			("C-n" . company-select-next)
			("C-p" . company-select-previous)
	)
	:config
		(global-company-mode)
)
(use-package company-quickhelp
	:after (company)
	:custom
		(company-quickhelp-delay 0)
	:hook (company-mode . company-quickhelp-mode)
)
(use-package company-quickhelp-terminal
	:after (company-quickhelp)
	:hook (company-mode . company-quickhelp-terminal-mode)
)
(use-package company-box
	:after (company)
	:custom
	(
		(company-box-show-single-candidate t)
  		(company-box-backends-colors nil) ;; Same colors for all backends
	)
	:custom-face
		(company-box-selection ((t (:inherit company-tooltip-selection :extend t))))
	:hook
		(company-mode . company-box-mode)
)
(use-package company-statistics
	:after (company)
	:custom (company-statistics-file (concat dir-cache "company-statistics-cache.el"))
	:hook (company-mode . company-statistics-mode)
)



;;;;;;;;;;;;;;;;;;;;;
;; Version Control ;;
;;;;;;;;;;;;;;;;;;;;;

(use-package git-gutter
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
		(global-git-gutter-mode)
)



;;;;;;;;;;;;;;;;;
;; Major Modes ;;
;;;;;;;;;;;;;;;;;

(use-package powershell)
(use-package toml-mode		:hook (yaml-mode . format-all-mode))
(use-package markdown-mode	:hook (markdown-mode . format-all-mode))

(use-package typescript-mode
	:custom
		(lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/tmp/tsserver.log"))
	:mode
		("\\.ts[x]?\\'" . typescript-mode)
	:ensure-system-package
	(
		(typescript-language-server . "yarn global add typescript-language-server")
		(tsc . "yarn global add typescript")
	)
)

(use-package yaml-mode
	:custom
	(
		(lsp-yaml-format-enable nil)
		(lsp-yaml-schemas t)
	)
	:ensure-system-package
		(yaml-language-server . "yarn global add yaml-language-server")
)

(use-package json-mode
	:custom (lsp-json-schemas t)
	:ensure-system-package
		(vscode-json-languageserver . "yarn global add vscode-json-languageserver")
)

(use-package graphql-mode
	:ensure-system-package
		(graphql-lsp . "yarn global add graphql-language-service-cli graphql")
)

(use-package php-mode
	:config
		(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)
		(setq php-template-compatibility nil)
		(setq php-lineup-cascaded-calls t)
)

(use-package terraform-mode
	:ensure-system-package
	(
		(terraform . terraform)
		(terraform-ls . "brew install hashicorp/tap/terraform-ls")
	)
)
(use-package company-terraform
	:after (terraform-mode company)
	:hook
		(terraform-mode . company-terraform-init)
)

(use-package org)
(use-package org-bullets :after (org-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Server Protocol ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
	:commands (lsp lsp-deferred)
	:ensure-system-package
		(bash-language-server . "yarn global add bash-language-server")
	:hook
	(
		(yaml-mode			. lsp-deferred)
		(json-mode			. lsp-deferred)
		(typescript-mode 	. lsp-deferred)
		(js-mode 			. lsp-deferred)
		(sh-mode			. lsp-deferred)
		(graphql-mode		. lsp-deferred)
		(terraform-mode		. lsp-deferred)

		(lsp-mode . lsp-enable-which-key-integration)
	)
	:custom
	(
		(lsp-auto-guess-root t)
		(lsp-eldoc-render-all nil)
		(lsp-session-file (concat dir-cache ".lsp-session-v1"))
		(lsp-keymap-prefix "s-l")
		(lsp-enable-indentation nil)
		(lsp-semantic-highlighting t)

		(lsp-bash-explainshell-endpoint t)
		(lsp-bash-highlight-parsing-errors t)
		(lsp-bash-glob-pattern t)
	)
	:config
	(lsp-register-client
		(make-lsp-client	:new-connection (lsp-stdio-connection '("terraform-ls" "serve"))
							:major-modes '(terraform-mode)
							:server-id 'terraform-ls
		)
	)
	(lsp-register-client
		(make-lsp-client	:new-connection (lsp-stdio-connection '("graphql-lsp" "server" "--method" "stream"))
							:major-modes '(graphql-mode)
							:server-id 'graphql-lsp
		)
	)
	(add-to-list 'lsp-language-id-configuration '(graphql-mode . "graphql"))
)
(use-package lsp-ui	:after (lsp-mode))

;(use-package dap-mode :after (lsp-mode))

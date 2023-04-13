;; -*- lexical-binding: t -*-

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-fringe-indicator-position 'right-fringe))

(customize-set-variable 'eldoc-echo-area-use-multiline-p nil)


(customize-set-variable 'gdb-many-windows t)

(use-package comment-dwim-2
  :defer 2
  :bind (("M-s-/" . comment-dwim-2)
	 :map org-mode-map
	 ("M-s-/" . org-comment-dwim-2)))


;;;;;;;;;;;;;;;;
;; Treesitter ;;
;;;;;;;;;;;;;;;;
(use-package treesit-auto
  :demand t
  :custom (treesit-auto-install 'prompt)
  :config (global-treesit-auto-mode))

;;;;;;;;;
;; LSP ;;
;;;;;;;;;
(use-package dap-mode
  :disabled
  :hook
  (dap-mode . acf/dap/setup)
  (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  :init
  (defun acf/dap/setup ()
    (dap-ui-mode 1)
    (dap-tooltip-mode 1)
    (tooltip-mode 1)
    (dap-ui-controls-mode 1))
  :custom
  (dap-auto-configure-features '(sessions locals controls tooltip)))


(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :bind (("M-<RET>" . lsp-execute-code-action)
         ("s-r r" . lsp-rename)
         ("M-s-<return>" . lsp-format-buffer))
  :custom
  (lsp-idle-delay 0)
  (lsp-enable-snippet nil)
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-provider :none)
  (lsp-diagnostics-provider :flymake)
  (lsp-headerline-breadcrumb-segments '(symbols))
  (lsp-enable-indentation nil)

  ;; To decide
  (lsp-restart 'ignore)
  (lsp-eldoc-enable-hover nil)
  (lsp-signature-auto-activate nil)
  (lsp-keep-workspace-alive nil)
  (lsp-auto-execute-action nil)
  (lsp-before-save-edits nil)
  (lsp-headerline-breadcrumb-enable nil)
  :init
  (defun acf/dap/csharp ()
    "Start dap-mode for csharp is available."
    (with-eval-after-load "dap-mode"
      (require 'dap-netcore)
      (dap-mode 1)))

  (defun acf/lsp/setup ()
    "Setup orderless completion."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults)) '(orderless))
    (lsp-enable-which-key-integration))
  :hook
  (lsp-completion-mode . acf/lsp/setup)
  ;;  (sh-mode . lsp-deferred)
  ;;  (conf-toml-mode . lsp-deferred)
  ;;  (c++-mode . lsp-deferred)
  ;;  (c-mode . lsp-deferred)
  ;;  (mhtml-mode . lsp-deferred)
  ;;  (css-mode . lsp-deferred)
  ;;  (csharp-mode . lsp-deferred)
  (csharp-ts-mode . lsp-deferred)
  ;; (csharp-ts-mode . acf/dap/csharp)
  ;;  (terraform-mode . lsp-deferred)
  ;;  (js-json-mode . lsp-deferred)
  ;;  (yaml-mode . lsp-deferred)
  ;;  (typescript-mode . lsp-deferred)
  ;;  (cmake-mode . lsp-deferred))
  )

(use-package lsp-ui
  :after lsp-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-peek-always-show t)
  (lsp-ui-peek-fontify 'always))

(use-package consult-lsp
  :requires consult
  :after lsp-mode
  :bind
  ([remap xref-find-apropos] . consult-lsp-symbols))


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
(use-package docker :disabled :bind ("C-c d" . docker))

;;;;;;;;;;;;;;;
;; Terraform ;;
(use-package terraform-mode
  :mode "\\.tf\\'"
  :ensure-system-package
  ((terraform)
   (terraform-ls)))

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


(use-package editorconfig :demand t :config (editorconfig-mode))




;; (use-package helpful
;;   :commands (helpful-callable helpful-variable helpful-command helpful-key)
;;   :bind
;;   ([remap describe-function] . helpful-callable)
;;   ([remap describe-command] . helpful-command)
;;   ([remap describe-variable] . helpful-variable)
;;   ([remap describe-key] . helpful-key))



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


(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :custom
  (copilot-idle-delay 0.5)
  :hook
  (prog-mode . copilot-mode))






(provide 'acf-programming)

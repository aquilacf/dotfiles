;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;
;; Company ;;
;;;;;;;;;;;;;
;; (use-package company
;;   :disabled
;;   :custom
;;   (company-idle-delay 0.5)
;;   (company-echo-delay 0)
;;   (company-show-numbers t)
;;   (company-selection-wrap-around t)
;;   (company-minimum-prefix-length 3)
;;   (company-tooltip-align-annotations t)
;;   (company-dabbrev-downcase nil)
;;   :config (global-company-mode))


;; (use-package company-statistics
;;   :after (company)
;;   :custom (company-statistics-file (concat DIR_CACHE "company-statistics-cache.el"))
;;   :config (company-statistics-mode))

;; (use-package company-box :hook (company-mode . company-box-mode))

;; (use-package company-quickhelp
;;   :after (company)
;;   :hook (company-mode . company-quickhelp-mode)
;;   :custom (company-quickhelp-delay 0))

;; (use-package company-quickhelp-terminal
;;   :after (company-quickhelp)
;;   :hook (company-quickhelp-mode . company-quickhelp-terminal-mode))


;;;;;;;;;;;
;; Minad ;;
;;;;;;;;;;;

(use-package vertico
  :custom (vertico-cycle t)
  :init (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :straight nil
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


(use-package marginalia
  :custom (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))


(use-package consult
  :custom (completion-in-region-function #'consult-completion-in-region))


(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '(file (styles . (partial-completion)))))


(use-package embark
  :bind
  (("C-." . embark-act)        
   ("C-;" . embark-dwim)       
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))




  
(use-package corfu
  :custom
   (corfu-cycle t)
   (corfu-auto t)
   (corfu-auto-prefix 2)
   (corfu-auto-delay 0.0)
   (corfu-echo-documentation 0.25)
  :init
  (global-corfu-mode))

(use-package corfu-history
  :after corfu
  :ensure nil
  :straight nil
  :config
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-doc
  :after corfu
  :hook (corfu-mode . corfu-doc-mode))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Terminal
;; (use-package popon
;;   :straight (:type git :host nil :repo "https://codeberg.org/akib/emacs-popon.git")
;;   :if (not (display-graphic-p)))

;; (use-package corfu-terminal
;;   :straight (:type git :host nil :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
;;   :after popon
;;   :init
;;   (global-corfu-terminal-mode))

;; (use-package corfu-doc-terminal
;;   :straight (corfu-doc-terminal :type git :host nil :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")
;;   :after corfu-terminal
;;   :hook (corfu-mode-terminal . corfu-doc-terminal-mode))



(provide 'my-completion)

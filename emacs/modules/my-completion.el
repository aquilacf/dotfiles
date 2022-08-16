;; -*- lexical-binding: t -*-

;;;;;;;;;;;
;; Minad ;;
;;;;;;;;;;;

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-reverse
				vertico-directory
                                ))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  :init
  (vertico-reverse-mode)
  (vertico-indexed-mode)
  
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
  :init
  (marginalia-mode))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))


(use-package consult
  :custom (completion-in-region-function #'consult-completion-in-region)
  (consult-narrow-key "<")
  :config
  (define-key minibuffer-local-map (kbd "C-r") 'consult-history)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))


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
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-history))
  :custom
   (corfu-cycle t)
   (corfu-auto t)
   (corfu-auto-prefix 2)
   (corfu-auto-delay 0.0)
   (corfu-echo-documentation 0.25)
  :init
  (global-corfu-mode)
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (svg-lib-icons-dir (concat DIR_CACHE "svg-lib/"))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(provide 'my-completion)

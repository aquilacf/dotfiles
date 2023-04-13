
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
  :straight (corfu :files (:defaults "extensions/*"))
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
  (corfu-popupinfo-delay '(2.0 . 0.0))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  (add-to-list 'savehist-additional-variables 'corfu-history)
  (unbind-key "RET" corfu-map)
  (unbind-key "M-SPC" corfu-map))

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :defer 2
  :straight (corfu-terminal
	     :type git
	     :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :config (corfu-terminal-mode))

(use-package kind-icon :after corfu :init (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'acf-completion)

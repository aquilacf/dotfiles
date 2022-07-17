;;;;;;;;;;;;;;;;;;
;; GC Threshold ;;
;;;;;;;;;;;;;;;;;;
(defconst GC_CONST_THREASHOLD_ORIGINAL gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)

; Empty file names list
(defconst FILE_NAME_HANDLER_ALIST_ORIGINAL file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'after-init-hook
		`(lambda ()
		   (setq gc-cons-threshold GC_CONST_THREASHOLD_ORIGINAL)
		   (setq file-name-handler-alist FILE_NAME_HANDLER_ALIST_ORIGINAL)
		   ) t)

(run-with-idle-timer 5 t #'garbage-collect)

;; Disable package
(setq package-enable-at-startup nil)

;; Define system constants
(defconst IS_MAC       (eq system-type 'darwin))
(defconst IS_LINUX     (eq system-type 'gnu/linux))
(defconst IS_WINDOWS   (memq system-type '(cygwin windows-nt ms-dos)))

;Clean up unnecessary option list.
(unless IS_MAC   (setq command-line-ns-option-alist nil))
(unless IS_LINUX (setq command-line-x-option-alist nil))

;; Define user constants
(defconst DIR_ROOT (expand-file-name user-emacs-directory))
(defconst DIR_CACHE (concat DIR_ROOT "cache/"))
(defconst DIR_SNIPPETS (concat DIR_ROOT "snippets/"))
(defconst DIR_PACKAGES (concat DIR_ROOT "packages/"))
(mkdir DIR_CACHE t)
;(mkdir DIR_SNIPPETS)

(fset 'yes-or-no-p 'y-or-n-p)


;;;;;;;;;;;;;;;;;;;;
;; Handling Files ;;
;;;;;;;;;;;;;;;;;;;;

;; Custom files
(setq custom-file (concat DIR_CACHE "custom.el"))
(load custom-file 'noerror 'nomessage)

(global-auto-revert-mode t)
(setq-default global-auto-revert-non-file-buffers t)
(setq create-lockfiles nil)

;; Charset
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-language-environment "UTF-8")
(setq default-input-method nil)
(setq require-final-newline t)

;; Recentf
(setq-default recentf-max-menu-items 25)
(setq-default recentf-max-saved-items 25)
(setq-default recentf-save-file (concat DIR_CACHE "recentf"))
(recentf-mode t)

;; Save Place
(setq-default save-place-file (concat DIR_CACHE "places"))
(save-place-mode t)

;; History
(setq history-length 30)
(setq-default savehist-file (concat DIR_CACHE "history"))
(savehist-mode t)
(setq-default url-history-file (concat DIR_CACHE "url-history"))

;; Backups
(defconst DIR_BACKUPS (concat DIR_ROOT "backups/"))
;(mkdir DIR_BACKUPS t)
(setq backup-inhibited t)
(setq make-backup-files nil)


;; Autosaves
(defconst DIR_AUTOSAVES (concat DIR_ROOT "autosaves/"))
;(mkdir DIR_AUTOSAVES t)
(setq auto-save-default nil)
(setq auto-save-interval 0)
(setq auto-save-timeout 0)
(setq auto-save-list-file-prefix nil)


;; Interface
(menu-bar-mode -1)
(tool-bar-mode -1)

(modify-all-frames-parameters '((fullscreen . maximized)
                                (internal-border-width . 1)
                                (vertical-scroll-bars . nil)
                                (tool-bar-lines . 0)
                                (ns-appearance . dark)
                                (font . "FiraCode Nerd Font-14")))



(defun display-startup-echo-area-message()
  "Message that will display on the footer when opening EMACS."
  (message (concat "Loaded in " (emacs-init-time) ".")))

(setq inhibit-startup-message t)
(setq inhibit-default-init t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)
(setq inhibit-splash-screen t)



;; Tramp
(setq tramp-persistency-file-name (concat DIR_CACHE "tramp"))
(setq-default tramp-default-method "ssh")

					; Projects
(setq project-list-file (concat DIR_CACHE "projects"))

;;;;;;;;;;;
;; Other ;;
;;;;;;;;;;;
(setq large-file-warning-threshold nil)
(setq vc-follow-symlinks t)
(setq idle-update-delay 1.0)
(setq inhibit-compacting-font-caches t)
(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)
(setq auto-mode-case-fold nil)
(setq-default bidi-display-reordering 'left-to-right
	      bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq-default ffap-machine-p-known 'reject)
(setq frame-inhibit-implied-resize t)


;;;;;;;;;;;;;;
;; Straight ;;
;;;;;;;;;;;;;;
(setq-default straight-base-dir DIR_CACHE)
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
(setq-default straight-use-package-by-default t
	      use-package-expand-minimally t
	      use-package-always-defer t) 

;;;;;;;;;;;
;; Theme ;;
;;;;;;;;;;;
(use-package doom-themes
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(defvar current-theme-style "d")
(defun my/toggle-theme ()
  "Toggle theme style."
  (interactive)
  (if (string-equal current-theme-style "d")
      (progn
	(disable-theme 'doom-one) (load-theme 'doom-one-light t) (setq current-theme-style "l"))
    (progn
      (disable-theme 'doom-one-light) (load-theme 'doom-one t) (setq current-theme-style "d"))))


(use-package doom-modeline
  :demand t
  :custom (doom-modeline-height 25)
  :config (doom-modeline-mode t))

;;; early-init.el ends here


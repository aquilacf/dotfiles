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
(defconst IS_TERMINAL  (not (display-graphic-p)))
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

;;;;;;;;;;;;;;;;;;;;
;; Handling Files ;;
;;;;;;;;;;;;;;;;;;;;

;; Custom files
(setq custom-file (concat DIR_CACHE "custom.el"))
(load custom-file 'noerror 'nomessage)

(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
(setq create-lockfiles nil)

;; Charset
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; Recentf
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(setq recentf-save-file (concat DIR_CACHE "recentf"))
(recentf-mode t)

;; Save Place
(setq save-place-file (concat DIR_CACHE "places"))
(save-place-mode t)

;; History
(setq history-length 30)
(setq savehist-file (concat DIR_CACHE "history"))
(savehist-mode t)


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
(defun display-startup-echo-area-message()
	"Message that will display on the footer when opening EMACS."
	(message (concat "Loaded in " (emacs-init-time) ".")))

(setq inhibit-startup-message t)
(setq inhibit-default-init t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)
(setq inhibit-splash-screen t)


(menu-bar-mode -1)
(unless IS_TERMINAL
	(scroll-bar-mode -1)
	(tool-bar-mode -1)
	(tooltip-mode -1))


(setq visible-bell t)
(setq use-dialog-box nil)

;; Line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(global-visual-line-mode)


;; Parenthesis
;(set-face-attribute 'show-paren-match nil :foreground 'unspecified :background 'unspecified :weight 'extra-bold :underline t)
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)


; Tramp
(setq tramp-default-method "ssh")

;;;;;;;;;;;;;;;;;;
;; Key Bindings ;;
;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

;; Mouse wheel
(global-set-key (kbd "<mouse-3>") (kbd "C-y"))
(global-set-key (kbd "<mouse-4>") (kbd "C-u 1 M-v"))
(global-set-key (kbd "<mouse-5>") (kbd "C-u 1 C-v"))

(global-unset-key (kbd "C-t")) ; This is reserved to tmux

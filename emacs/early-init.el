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

;; Compilation cache
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
	  (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; Disable package
(setq package-enable-at-startup nil)

;; Define system constants
(defconst IS_MAC       (eq system-type 'darwin))
(defconst IS_LINUX     (eq system-type 'gnu/linux))
(defconst IS_WINDOWS   (memq system-type '(cygwin windows-nt ms-dos)))

;Clean up unnecessary option list.
(unless IS_MAC   (setq command-line-ns-option-alist nil))
(unless IS_LINUX (setq command-line-x-option-alist nil))

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
  (message (concat "Loaded in " (emacs-init-time) ".")))

(setq inhibit-startup-message t)
(setq inhibit-default-init t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)
(setq inhibit-splash-screen t)

;;;;;;;;;;;;;;
;; Straight ;;
;;;;;;;;;;;;;;
(unless (featurep 'straight)
  ;; Bootstrap straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;; Use Package
(straight-use-package 'use-package)
(straight-use-package 'use-package-ensure-system-package)
(custom-set-variables '(straight-use-package-by-default t)
		      '(use-package-expand-minimally t)
		      '(use-package-always-defer t))

;;;;;;;;;;;
;; Theme ;;
;;;;;;;;;;;
(use-package doom-themes
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-monokai-pro t)
  (doom-themes-org-config))

(use-package doom-modeline
  :demand t
  :custom (doom-modeline-height 30)
  :config (doom-modeline-mode t))

;;; early-init.el ends here
1

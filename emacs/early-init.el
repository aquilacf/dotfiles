;;; early-init.el --- Emacs early init.

;;; Commentary:
;; Modifies GC, disables package, defines contants, etc...

;; -*- lexical-binding: t -*-

;;; Code:

;;;;;;;;;;;;;;;;;;
;; GC Threshold ;;
;;;;;;;;;;;;;;;;;;
(setq gc-cons-threshold 100000000)      ; 100mb
(setq read-process-output-max 1048576)  ; 1mb

;; Disable package
(setq package-enable-at-startup nil)

;; Define system constants
(defconst IS_MAC       (eq system-type 'darwin))
(defconst IS_LINUX     (eq system-type 'gnu/linux))
(defconst IS_WINDOWS   (memq system-type '(cygwin windows-nt ms-dos)))

;Clean up unnecessary option list.
(unless IS_MAC   (setq command-line-ns-option-alist nil))
(unless IS_LINUX (setq command-line-x-option-alist nil))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;
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

;;; early-init.el ends here

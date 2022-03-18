;;; init.el --- EMACS
;;; Commentary: Requires >26.1
;;; Code:

;(setq gc-cons-threshold (* 1024 1024 100))
;(setq file-name-handler-alist nil)

;;;;;;;;;;;;;;;
;; Load core ;;
;;;;;;;;;;;;;;;

(load (concat user-emacs-directory "core/main"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup package repositories ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
						 ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package)
)

;; Start package manager @todo: is this really necessary? it's causing error process sentinel: args out of range with find-file-hook
;(eval-when-compile
;	(require 'use-package)
(core:init-packages)
;)

;;; init.el ends here

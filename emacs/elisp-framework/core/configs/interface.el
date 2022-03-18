;;; interface.el --- Interface definitions.
;;; Commentary:
;;; Code:

;; Interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(transient-mark-mode t)
(xterm-mouse-mode t)	; Enable mouse.
(setq visible-bell t)	; Disable annoying bell.


; (setq redisplay-dont-pause t
;   scroll-margin 1
;   scroll-step 1
;   scroll-conservatively 10000
;   scroll-preserve-screen-position 1)

;; Smoother and nicer scrolling
; (setq scroll-margin 10
;    scroll-step 1
;    next-line-add-newlines nil
;    scroll-conservatively 10000
;    scroll-preserve-screen-position 1)

; Enable line numbers >v26
(global-display-line-numbers-mode)
(global-visual-line-mode)

; Change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

; Set unicode
;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)) ; Treat clipboard input as UTF-8 string first.
(set-buffer-file-coding-system 'utf-8-unix)



;; Highlight matching parenthesis
(set-face-attribute 'show-paren-match nil :foreground 'unspecified :background 'unspecified :weight 'extra-bold :underline t)
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis) ; 'expression , 'parenthesis , 'mixed


;; Startup motivational message
(defun display-startup-echo-area-message()
  "Message that will display on the footer when opening EMACS.")
;	(message "Let the hacking begin!"))


;; No welcome screen
(setq	inhibit-startup-message t
		initial-scratch-message ""
		initial-major-mode 'text-mode
		inhibit-splash-screen t)


												;; Defaults for identation/trailing:
(setq-default 	delete-trailing-lines t			; 	Delete useless empty lines at bottom.
				delete-trailing-whitespace t	; 	Delete useless whitespaces.
				indent-tabs-mode t				;	Tabs, not spaces.
				tab-always-indent 'complete		;	Force tab insertion.
				tab-width 4)					;	Tab with.
												; Editorconfig can override this.
(electric-indent-mode -1)
(setq-default backward-delete-char-untabify-method nil)



; (setq whitespace-style '(face tabs tab-mark trailing))
; (custom-set-faces
;  '(whitespace-tab ((t (:foreground "#636363")))))
; (setq whitespace-display-mappings
;   '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
; (global-whitespace-mode)




; font


(provide 'interface)

;;; interface.el ends here

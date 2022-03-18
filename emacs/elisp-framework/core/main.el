;; -*- lexical-binding: t -*-
;;; main.el --- Core of the framework.
;;; Commentary: This should be loaded first and once.
;;; Code:

;; Remove security vulnerability
(eval-after-load "enriched"
	'(defun enriched-decode-display-prop (start end &optional param)
		(list start end)
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize global variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq dir-root (expand-file-name user-emacs-directory))				; .emacs.d absolute path
(setq dir-configs (concat dir-root "core/configs/"))				; Configuration Settings
(setq dir-core (concat dir-root "core/"))							; Path of this directory

																	; Absolute Paths to:
(setq dir-modes (concat dir-root "modes/"))							;  - Custom Modes
(setq dir-cache (concat dir-root ".cache/"))						;  - Cache
(setq dir-backups (concat dir-root ".backups/"))					;  - Backups
(setq dir-autosaves (concat dir-root ".autosaves/"))				;  - Autosaves
;(setq dir-snippets (concat dir-root "snippets/"))					;  - Snippets @todo: yasnippet
(setq package-user-dir (concat dir-cache "elpa/"))					;  - ELPA custom location
(setq custom-file (concat dir-cache "priv.custom.el"))				;  - Move 'customcustom-set-variables'




;;;;;;;;;;;;;;;;;;;;
;; Core functions ;;
;;;;;;;;;;;;;;;;;;;;

(defun core:load-directory (dir)

	"Load all elips files in dir, .elc version will be loaded over .el. Do not delete .el files."

	(let ((load-it (lambda (f)
						(load (concat (file-name-as-directory dir) (file-name-base f))))
				 ))
		(mapc load-it (directory-files dir nil "\\.el$"))
	)

)



(defun core:download (url p)

	"Download any file from the internet.  It doesn't override the existing file."

	(unless (file-exists-p p)
		(url-retrieve	url
						(lambda (s)
							(re-search-forward "\r?\n\r?\n")
							(write-region (point) (point-max) p)
						)
		)

    )

)



(defun core:load-mode ()

	"Initialize mode based on the file extension. Mode most exist in modes/.el."

	(let (f)
		(setq f (concat dir-modes (file-name-extension buffer-file-name)))
		(if (file-exists-p (concat f ".el")) (load f))
	)

)



(defun core:compile()

	"Compile all .el files in the project after first run."

	(let (f)
		(setq f (concat dir-cache "priv.compiled"))
		(unless (file-exists-p f)
			;(byte-recompile-directory root)
			(write-region nil "" f)
		)
	)

)



(defun core:mkdir(dir)

	"Create a directory, skip if directory already existent."

	(unless (file-directory-p dir) (make-directory dir))

)



(defun core:is-font-installed (name)

	"Verify if font is installed in the system."

	(if (find-font (font-spec :name name)) t nil)

)



;;;;;;;;;;;;;;;;;;;;
;; Mode functions ;;
;;;;;;;;;;;;;;;;;;;;

(defun mode:initialize (mode)

	"Initialize a major mode. Currently not being in use."

	(let (dir-mode)
		(setq dir-mode (concat dir-modes mode "/"))
		(set (intern (concat "dir-mode-" mode)) dir-mode)	; This line creates a dynamic variable dir-mode-'X' where 'X' is the mode. Tip from: https://www.rosettacode.org/wiki/Dynamic_variable_names#Emacs_Lisp
		(core:mkdir dir-mode)
    )

)



;;;;;;;;;;;;;;;;;;;;
;; Init functions ;;
;;;;;;;;;;;;;;;;;;;;

(defun core:init-packages ()

	"Initialize main packages."

	(load (concat dir-core "packages"))

)



;;;;;;;;;;;;;;;;;;;
;; Load Settings ;;
;;;;;;;;;;;;;;;;;;;

(core:load-directory dir-configs)


;;; main.el ends here

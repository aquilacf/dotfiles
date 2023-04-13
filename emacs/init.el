(unless (featurep 'straight)
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

(straight-use-package 'use-package)
(custom-set-variables '(straight-use-package-by-default t)
		      '(use-package-expand-minimally t)
		      '(use-package-always-defer t))

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'acf-defaults)
(require 'acf-ui)
(require 'acf-completion)
(require 'acf-project)
(require 'acf-programming)
(require 'acf-pdf)

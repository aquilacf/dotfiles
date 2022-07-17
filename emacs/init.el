;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat DIR_ROOT "modules"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieve PATH from zshrc ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/update-path-from-zshell()
  "Update exec-path and PATH with zshrc PATHs."
  (let ((path (shell-command-to-string "TERM=dumb . ~/.zshrc; echo -n $PATH")))
    (setenv "PATH" path)
    (setq exec-path (split-string-and-unquote path path-separator))))

(when (display-graphic-p)
  (my/update-path-from-zshell))

(require 'my-defaults)
(require 'my-editing)
(require 'my-project)
(require 'my-ui)
(require 'my-completion)
(require 'my-ide)


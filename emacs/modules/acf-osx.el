(defun acf/osx/update-path-from-zshell()
  "Update exec-path and PATH with zshrc PATHs."
  (let ((path (shell-command-to-string "TERM=dumb . ~/.zshrc; echo -n $PATH")))
    (setenv "PATH" path)
    (setq exec-path (split-string-and-unquote path path-separator))))

(provide 'acf-osx)

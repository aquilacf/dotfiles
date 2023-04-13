
(use-package xterm-color
  :demand t
  :custom
  (compilation-environment '("TERM=xterm-256color"))
  :init
  (defun acf/compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  :config
  (advice-add 'compilation-filter :around #'acf/compilation-filter))


(use-package eat
  :straight (:host codeberg
             :repo "akib/emacs-eat"
             :files ("*.el" ("term" "term/*.el") "*.texi"
                     "*.ti" ("terminfo/e" "terminfo/e/*")
                     ("terminfo/65" "terminfo/65/*")
                     ("integration" "integration/*")
                     (:exclude ".dir-locals.el" "*-tests.el"))))


(provide 'acf-shell)

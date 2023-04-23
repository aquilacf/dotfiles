
(use-package xterm-color
  :demand t
  :custom
  (compilation-environment '("TERM=xterm-256color"))
  :init
  (defun acf/compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  :config
  (advice-add 'compilation-filter :around #'acf/compilation-filter))


(provide 'acf-shell)

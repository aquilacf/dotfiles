

(use-package pdf-tools
  :hook (doc-view-mode . (lambda () (require 'pdf-tools))))


(with-eval-after-load 'pdf-tools
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width))

(provide 'my-pdf)

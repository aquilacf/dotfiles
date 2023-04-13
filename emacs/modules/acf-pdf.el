;;;;;;;;;
;; PDF ;;
;;;;;;;;;
(use-package pdf-tools :hook (doc-view-mode . (lambda () (require 'pdf-tools))))

(provide 'acf-pdf)

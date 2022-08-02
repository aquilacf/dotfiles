;; -*- lexical-binding: t -*-

(defun my/save-all-buffers ()
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'my/save-all-buffers)



(customize-set-variable 'warning-minimum-level :emergency)




(provide 'my-defaults)

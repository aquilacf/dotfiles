(customize-set-variable 'crafted-ui-default-font '(:font "FiraCode Nerd Font" :height 140))
(add-to-list 'default-frame-alist '(fullscreen . maximized))


(custom-set-variables '(crafted-startup-inhibit-splash t)
		      '(inhibit-default-init t)
		      '(initial-scratch-message nil))



;;; Package -----------

;; Initialize package to load doom-themes before frame shows up.
(package-initialize)

;; Don't let Crafted Emacs initialize package again.
(customize-set-variable 'package-enable-at-startup nil)

;;; Package -----------

(when (require 'doom-themes nil 'noerror)
  (progn
    (disable-theme 'deeper-blue)
    (load-theme 'doom-one t)))

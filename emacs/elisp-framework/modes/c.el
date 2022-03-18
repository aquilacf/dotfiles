;;; cpp.el --- C/C++ configs
;;; Commentary:
;;; Code:

; Initializes the mode
(mode:initialize "c")

; Download google cpplint from github
(core:download "https://raw.githubusercontent.com/google/styleguide/gh-pages/cpplint/cpplint.py" (concat dir-mode-c "priv.cpplint.py"))
(core:download "https://raw.githubusercontent.com/google/styleguide/gh-pages/google-c-style.el" (concat dir-mode-c "priv.google-c-style.el"))


;;@todo: add flycheck





; Include flycheck-google-cppling since it's not on MELPA
;(load (concat user-emacs-directory "c/flycheck-google-cpplint"))
;(custom-set-variables '(flycheck-c/c++-googlelint-executable "c/cpplint.py")
;		      '(flycheck-google-cpplint-verbose "3")
;		      '(flycheck-googlelint-filter "-whitespace,+whitespace/braces")
;		      '(flycheck-googlelint-linelength "120"))
;(flycheck-add-next-checker 'c/c++-googlelint)





;;; c.el ends here

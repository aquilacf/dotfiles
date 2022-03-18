(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq package-enable-at-startup nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 67108864
                   gc-cons-percentage 0.1
                   file-name-handler-alist file-name-handler-alist-original)
             (garbage-collect)) t)


(menu-bar-mode -1)


(defconst dir-root (expand-file-name user-emacs-directory))
(defconst dir-cache (concat dir-root "cache/"))
(defconst dir-backups (concat dir-root "backups/"))
(defconst dir-autosaves (concat dir-root "autosaves/"))
(defconst dir-snippets (concat dir-root "snippets/"))

(setq package-user-dir (concat dir-cache "elpa/"))					;  - ELPA custom location
(setq custom-file (concat dir-cache "priv.custom.el"))				;  - Move 'customcustom-set-variables'


(setq create-lockfiles nil)
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)


(defun display-startup-echo-area-message()

(message (concat "Loaded in " (emacs-init-time) ".")))


(setq	inhibit-startup-message t
		inhibit-default-init t
		initial-scratch-message nil
		initial-major-mode 'fundamental-mode
		inhibit-splash-screen t)


(setq auto-mode-case-fold nil)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq ffap-machine-p-known 'reject)
(setq frame-inhibit-implied-resize t)


(setq idle-update-delay 1.0)
(setq inhibit-compacting-font-caches t)
(setq redisplay-skip-fontification-on-input t)


(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(unless IS-MAC   (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))





(global-display-line-numbers-mode)
(global-visual-line-mode)
(fset 'yes-or-no-p 'y-or-n-p)

(set-face-attribute 'show-paren-match nil :foreground 'unspecified :background 'unspecified :weight 'extra-bold :underline t)
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)



(xterm-mouse-mode t)







(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))
(global-set-key (kbd "<mouse-3>") (kbd "C-y"))
(global-set-key (kbd "<mouse-4>") (kbd "C-u 1 M-v"))
(global-set-key (kbd "<mouse-5>") (kbd "C-u 1 C-v"))

(global-unset-key (kbd "C-t"))				; Reserved for tmux.






(global-auto-revert-mode t)


(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;;; acf-org.el --- User ORG module.

;;; Commentary:
;; This module contains styling and eye candy for org.

;; -*- lexical-binding: t -*-

;;; Code:


(use-package org-modern
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)

  ;; Org styling, hide markup etc.
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis "…")

  ;; Agenda styling
  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid '((daily today require-timed)   (800 1000 1200 1400 1600 1800 2000)   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string "⭠ now ─────────────────────────────────────────────────")
  :custom-face
  (org-modern-symbol ((nil (:family "FiraCode Nerd Font"))))
  :config
  (global-org-modern-mode))

(provide 'acf-org)

;;; acf-org.el ends here

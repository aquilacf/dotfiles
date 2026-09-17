(require (prefix-in project-hx. "project-hx/project.scm"))
(require (prefix-in git. "cogs/git.scm"))

(provide git-blame git-diff-hunk forgelink-copy forgelink-open project-picker)

(project-hx.project-ignore! '("*/node_modules/*" "*/target/*" "*/.cargo/*"))
(project-hx.project-enable-auto-save!)

;;@doc
;; Blame the lines covered by the primary selection
(define (git-blame)
  (git.blame))

;;@doc
;; Show the diff hunk under the cursor, including unsaved changes
(define (git-diff-hunk)
  (git.diff-hunk))

;;@doc
;; Copy a forge link to the selected lines
(define (forgelink-copy)
  (git.link-copy))

;;@doc
;; Open a forge link to the selected lines
(define (forgelink-open)
  (git.link-open))

;;@doc
;; Pick a saved project and open it
(define (project-picker)
  (project-hx.project-picker))

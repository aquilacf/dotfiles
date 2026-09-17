(require "helix/editor.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(require-builtin helix/core/text)
(require-builtin steel/time)

(provide blame diff-hunk link-copy link-open)

(define (current-doc-id)
  (editor->doc-id (editor-focus)))

(define (current-path)
  (editor-document->path (current-doc-id)))

(define (primary-range)
  (helix.static.selection->primary-range (helix.static.current-selection-object)))

(define (char->line char-idx)
  (rope-char->line (editor->text (current-doc-id)) char-idx))

(define (selection-line-start)
  (+ 1 (char->line (helix.static.range->from (primary-range)))))

(define (selection-line-end)
  (max (selection-line-start) (char->line (helix.static.range->to (primary-range)))))

(define (blame)
  (helix.run-shell-command
   (string-append "git blame -L"
                  (int->string (selection-line-start))
                  ","
                  (int->string (selection-line-end))
                  " "
                  (current-path))))

(define (forgelink action)
  (helix.run-shell-command
   (string-append "forgelink "
                  action
                  " \""
                  (current-path)
                  ":"
                  (int->string (selection-line-start))
                  "-"
                  (int->string (selection-line-end))
                  "\"")))

(define (link-copy)
  (forgelink "copy"))

(define (link-open)
  (forgelink "open"))

;; The buffer is written to a temp file so the diff reflects unsaved changes.
(define (buffer->temp-file)
  (let* ([path (string-append "/tmp/hx-diff-hunk-" (int->string (current-milliseconds)))]
         [port (open-output-file path)])
    (write-string (rope->string (editor->text (current-doc-id))) port)
    (close-output-port port)
    path))

;; Diffs the buffer against HEAD and keeps only the hunk whose new-side range
;; covers `line`. The temp files are removed by the command itself because
;; `run-shell-command` runs asynchronously.
(define (hunk-command path line buffer-file)
  (string-append
   "cd \"$(dirname '" path "')\" && "
   "git show \"HEAD:./$(basename '" path "')\" > '" buffer-file ".base' 2>/dev/null; "
   "diff -u '" buffer-file ".base' '" buffer-file "' | awk -v l=" (int->string line) " '"
   "/^@@/ { split($3,a,/,/); s=substr(a[1],2)+0; n=(length(a[2])==0?1:a[2]+0); if(n==0)n=1; show=(l>=s && l<s+n) } "
   "show { print; p=1 } "
   "END { if (!p) print \"No changes at line \" l }"
   "'; rm -f '" buffer-file "' '" buffer-file ".base'"))

(define (diff-hunk)
  (helix.run-shell-command
   (hunk-command (current-path) (selection-line-start) (buffer->temp-file))))

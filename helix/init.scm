(require (only-in "project-hx/project.scm"
                  project-add
                  project-remove
                  project-open
                  project-picker))
(require "helix/keymaps.scm")
(require "helix-file-watcher/file-watcher.scm")

(keymap (global)
        (normal (space (space (g (b ":git-blame")
                                 (d ":git-diff-hunk")
                                 (c ":forgelink-copy")
                                 (o ":forgelink-open"))))))

(spawn-watcher)

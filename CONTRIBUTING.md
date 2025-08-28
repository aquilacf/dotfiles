### CONTRIBUTING — Guidelines for humans and AI agents

This document explains the repository structure, contribution rules, scripting standards, and how to work with submodules. For agent-specific operating rules, see `AGENTS.md`. For an overview and getting started, see `README.md`.

### Repository structure (high level)
- `alacritty/` — terminal config and theme submodule (`alacritty-theme`).
- `bin/` — POSIX `sh` utility scripts (installation, setup, helpers).
- `brew/` — `Brewfile` for optional package setup.
- `git/` — Git configuration snippets.
- `gnupg/` — GnuPG configuration (public; secrets are not stored here).
- `helix/` — Helix editor config, themes, and the Helix source submodule in `helix/src`.
- `latex/` — LaTeX files (e.g., CV).
- `ssh/` — SSH configuration (public; keys are not stored here beyond public material).
- `tmux/` — tmux configuration.
- `zellij/` — zellij configuration.
- `vscode/` — VS Code settings and keybindings.
- `zsh/` — Zsh config and submodules.

### Public repository and private files
- This repository is public. Do not commit secrets or sensitive data.
- Private files must be prefixed with `priv.`. They are ignored by Git via the root `.gitignore`.
- Exception policy for ignored files: only files prefixed `priv.` may be intentionally created/read/edited while ignored.
- Some directories contain their own `.gitignore`. These rules apply only to that directory and its subdirectories.

### Platform and paths
- Target platform is macOS (Intel and Apple Silicon). Contributions should work on both.
- Avoid absolute paths. Prefer XDG base directories and environment-derived paths.
- When referencing Homebrew, prefer `$HOMEBREW_PREFIX` rather than hard-coded prefixes.

### Shell and scripting standards
- Interactive shell of choice: Zsh.
- All repository scripts must be POSIX-compliant `sh` with shebang `#!/bin/sh` and valid `dash` syntax.
- Do not add comments to shell scripts unless explicitly requested by the repository owner.

### Submodules
The repository uses Git submodules:
- `alacritty/alacritty-theme`
- `helix/src` (Helix upstream source)
- `zsh/zsh-autosuggestions`
- `zsh/zsh-defer`
- `zsh/zsh-syntax-highlighting`
- `zellij/website`

Clone with submodules:
```sh
git clone --recurse-submodules <repo-url>
```

Initialize/update submodules after cloning:
```sh
git submodule update --init --recursive
```

Update submodules to their recorded commits later:
```sh
git submodule update --recursive --remote
```

### Scripts and their roles
- `bin/install_dotfiles.sh`
  - Creates XDG directories, sets `~/.zshenv`, and symlinks config directories into `~/.config`.
  - Symlinks SSH config and the public key stub.
- `bin/install_helix.sh`
  - Installs Helix from source (`helix/src`) using Cargo and sets `HELIX_DEFAULT_RUNTIME`.
  - Symlinks shell completions for Zsh.
- `bin/macos_defaults.sh`
  - Applies opinionated macOS defaults (keyboard, Dock, Time Machine, Spaces behavior).
- `bin/test_true_colours.sh`
  - Renders a 24-bit color test pattern in the terminal.
- `bin/auto-retry.sh`
  - Re-runs a command until it exits successfully.

### Editor and related rules
- Editor of choice is Helix. Configuration lives under `helix/`.
- Before editing Helix configuration files (`helix/config.toml`, `helix/languages.toml`, or themes under `helix/themes/`), consult the Helix documentation in `helix/src/book/`.

### Git and SSH
- SSH is configured to use the GPG SSH Agent. GPG is configured for a YubiKey.
- The SSH host alias `git.aquilafreitas.com` maps to `github.com` (see `ssh/config`). Use that alias when applicable.

### Contribution workflow
- Keep changes minimal, focused, and human-ergonomic. Avoid over-engineering.
- Follow the platform and scripting standards above.
- For significant changes, document rationale in commit messages.
- For agents: follow the planning, safety, and logging rules in `AGENTS.md`.



### dotfiles

Personal dotfiles for macOS, used across Intel and Apple Silicon machines. This repository is public and avoids secrets. It configures the terminal stack (Alacritty, tmux, Zsh) and the Helix editor.

### Highlights
- macOS-focused, works on Intel and ARM
- Alacritty terminal with theme submodule
- tmux configuration
- Zsh as the interactive shell
- Helix editor configured and buildable from source
- Optional Homebrew bundle via `brew/Brewfile`

### Quick start
Prerequisites:
- macOS (Intel or Apple Silicon)
- Git, Homebrew (recommended)

Install the dotfiles (creates XDG dirs, sets `~/.zshenv`, and symlinks configs):
```sh
sh bin/install_dotfiles.sh
```

Optional: install Helix from source (uses `helix/src` and Cargo):
```sh
sh bin/install_helix.sh
```

### Notes
- SSH is configured to use the GPG SSH Agent; the host alias `git.aquilafreitas.com` maps to `github.com`.

### Contributing
For repository rules, structure details, scripting standards, and submodule instructions, see `CONTRIBUTING.md`.

### Contact
Owner: Áquila Freitas — hi@aquilafreitas.com



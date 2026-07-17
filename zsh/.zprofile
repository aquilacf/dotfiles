### ~/.config/zsh/.zprofile

## Homebrew
eval "$(brew shellenv)"

## Basic
export PATH="${PATH}:${HOME}/.bin"

## DOTNET
export PATH="${PATH}:${HOME}/.dotnet/tools"

## CARGO
export PATH="${PATH}:${HOME}/.cargo/bin"
export PATH="${PATH}:${HOMEBREW_PREFIX}/opt/rustup/bin"
export PATH="${PATH}:${HOMEBREW_PREFIX}/opt/llvm/bin"
export CARGO_TARGET_DIR="$HOME/.cargo/shared_target"

## Custom scripts
export PATH="${PATH}:${XDG_CONFIG_HOME}/zsh/bin"

## GPG
gpg-agent --daemon --pinentry-program "$HOMEBREW_PREFIX/bin/pinentry-mac" &>/dev/null
export GPG_SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
export GPG_KEYID="0xCA0901B5B1EDADB3"

# PIPX
export PATH="${PATH}:${HOME}/.local/bin"

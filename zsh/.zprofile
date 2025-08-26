### ~/.config/zsh/.zprofile

## Homebrew
eval "$(brew shellenv)"

## GPG
gpg-agent --daemon --pinentry-program "$HOMEBREW_PREFIX/bin/pinentry-mac" &>/dev/null
export GPG_SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
export GPG_KEYID="0xCA0901B5B1EDADB3"
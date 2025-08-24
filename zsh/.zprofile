### ~/.config/zsh/.zprofile

## Homebrew
eval "$(brew shellenv)"

## GPG
export GPG_TTY=$(tty)
export GPG_KEYID="0xCA0901B5B1EDADB3"
export GPG_SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
gpgconf --launch gpg-agent

### ~/.zprofile

## Homebrew
eval "$(/usr/local/bin/brew shellenv)"

## GPG
# export GPG_TTY=$(tty)
export GPG_KEYID="0xCA0901B5B1EDADB3"

if [[ -z "$gnupg_SSH_AUTH_SOCK_by" ]]; then
    export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
    export gnupg_SSH_AUTH_SOCK_by=$$
    # Launch GPG agent
    gpgconf --launch gpg-agent
fi




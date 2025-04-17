# Set GPG TTY
set -gx GPG_TTY (tty)

# Set GPG Key ID
set -gx GPG_KEYID 0xCA0901B5B1EDADB3

# Set SSH_AUTH_SOCK if not already set by this process
if not set -q gnupg_SSH_AUTH_SOCK_by
    set -gx SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
    set -gx gnupg_SSH_AUTH_SOCK_by %self

    # Launch GPG agent
    gpgconf --launch gpg-agent
end

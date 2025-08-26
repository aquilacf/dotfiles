### ~/.config/zsh/zshenv

## XDG
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_STATE_HOME="${HOME}/.local/state"
export XDG_DATA_HOME="${HOME}/.local/share"

## Basic
export PATH="$PATH:$HOME/.bin"
export LANG='en_GB.UTF-8'

## GNU
export GNUPGHOME="$XDG_CONFIG_HOME/gnupg"
export GPG_TTY=$(tty)

## DOTNET
export DOTNET_CLI_TELEMETRY_OPTOUT="true"
# export PATH="$PATH:/usr/local/share/dotnet"
# export PATH="$PATH:$HOME/.dotnet/tools"

## Cargo
source "$HOME/.cargo/env"

## Docker
export COMPOSE_BAKE="true"

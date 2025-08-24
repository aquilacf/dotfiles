### ~/.config/zsh/zshenv

echo "inside zshenv"


## Basic
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_STATE_HOME="${HOME}/.local/state"
export XDG_DATA_HOME="${HOME}/.local/share"

## PATH
export PATH="$PATH:$HOME/.bin"

## GNU
export GNUPGHOME="$XDG_CONFIG_HOME/gnupg"

## DOTNET
export DOTNET_CLI_TELEMETRY_OPTOUT="true"
# export PATH="$PATH:/usr/local/share/dotnet"
# export PATH="$PATH:$HOME/.dotnet/tools"

## Cargo
source "$HOME/.cargo/env"

## Docker
export COMPOSE_BAKE="true"

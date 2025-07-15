### ~/.zshenv

# DOTNET
export DOTNET_CLI_TELEMETRY_OPTOUT="true"
# export PATH="$PATH:/usr/local/share/dotnet"
# export PATH="$PATH:$HOME/.dotnet/tools"

## PATH
export PATH="$PATH:$HOME/.bin"

# Cargo
source "$HOME/.cargo/env"

## Docker
export COMPOSE_BAKE="true"

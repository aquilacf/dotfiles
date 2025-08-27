#!/bin/sh
set -e

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

HELIX="$SCRIPT_DIR/../helix/src"
export HELIX_DEFAULT_RUNTIME="$HELIX/runtime/"

cargo install -vv --path "$HELIX/helix-term" --locked

# Install global npm packages
echo "Installing global npm packages..."
npm install -g tombi

# Install brew programs
echo "Installing brew programs..."
brew tap hashicorp/tap
brew install hashicorp/tap/terraform
brew install hashicorp/tap/terraform-ls
brew install sql-language-server
brew install shellcheck
brew install superhtml
brew install typescript-language-server
brew install typescript
brew install yaml-language-server
brew install vscode-langservers-extracted
brew install hashicorp/tap/terraform

# Autocompletions
cd "$SCRIPT_DIR/../zsh/completions"
ln -fs "../../helix/src/contrib/completion/hx.zsh" "_hx"

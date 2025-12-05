#!/bin/sh
set -e

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

HELIX="$SCRIPT_DIR/../helix/src"
export HELIX_DEFAULT_RUNTIME="$HELIX/runtime/"

cargo install -vv --path "$HELIX/helix-term" --locked

# Install additional cargo tools
echo "Installing additional cargo tools..."
cargo install jinja-lsp --locked
cargo install --path "$SCRIPT_DIR/../harper/src/harper-ls" --locked
cargo install beancount-language-server --locked

# Install global npm packages
echo "Installing global npm packages..."
npm i -g dockerfile-language-server-nodejs
npm i -g @microsoft/compose-language-service
npm i -g @tailwindcss/language-server
npm i -g bash-language-server

# Install brew programs
echo "Installing brew programs..."
brew install hashicorp/tap/terraform
brew install hashicorp/tap/terraform-ls
brew install hougesen/tap/kdlfmt
brew install sql-language-server
brew install sql-formatter
brew install shellcheck
brew install superhtml
brew install typescript-language-server
brew install typescript
brew install yaml-language-server
brew install vscode-langservers-extracted
brew install llvm
brew install taplo
brew install ltex-ls-plus
brew install markdown-oxide
brew install marksman
brew install beancount

# Autocompletions
cd "$SCRIPT_DIR/../zsh/completions"
ln -fs "../../helix/src/contrib/completion/hx.zsh" "_hx"

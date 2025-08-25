#!/bin/sh
set -e

HELIX="$(pwd)/../helix/src"
export HELIX_DEFAULT_RUNTIME="$HELIX/runtime/"

cargo install -vv --path "$HELIX/helix-term" --locked

# Autocompletions
cd ../zsh/completions
ln -fs "../../helix/src/contrib/completion/hx.zsh" "_hx"

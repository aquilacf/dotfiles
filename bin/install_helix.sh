#!/bin/sh
set -e

HELIX="$(pwd)/../helix/src"
export HELIX_DEFAULT_RUNTIME="$HELIX/runtime/"

cargo install -vv --path "$HELIX/helix-term" --locked
ln -fs "$HELIX/contrib/completion/hx.zsh" "../zsh/completions/_hx"

#!/usr/bin/env zsh

set -e


HELIX="$(pwd)/helix"
export HELIX_DEFAULT_RUNTIME="$HELIX/runtime/"


cargo install -vv --path "$HELIX/helix-term" --locked
ln -fs "$HELIX/contrib/completion/hx.zsh" "../zsh/completions/_hx"

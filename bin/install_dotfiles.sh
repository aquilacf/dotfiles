#!/bin/sh
set -e

echo "Creating ~/.zshenv..."
cat > "$HOME/.zshenv" << 'EOF'
export ZDOTDIR="$HOME/.config/zsh"
source "$ZDOTDIR/.zshenv"
EOF

echo "Creating ~/.hushlogin..."
touch "$HOME/.hushlogin"

echo "Creating XDG directories..."
mkdir -p "$HOME/.config"
mkdir -p "$HOME/.cache"
mkdir -p "$HOME/.local/state"
mkdir -p "$HOME/.local/share"

echo "Linking dotfile directories to ~/.config..."
CONFIG_DIR="$HOME/.config/"
DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"

ln -sf "$DOTFILES_DIR/alacritty" "$CONFIG_DIR"
ln -sf "$DOTFILES_DIR/git" "$CONFIG_DIR"
ln -sf "$DOTFILES_DIR/gnupg" "$CONFIG_DIR"
ln -sf "$DOTFILES_DIR/helix" "$CONFIG_DIR"
ln -sf "$DOTFILES_DIR/tmux" "$CONFIG_DIR"
ln -sf "$DOTFILES_DIR/zellij" "$CONFIG_DIR"
ln -sf "$DOTFILES_DIR/zsh" "$CONFIG_DIR"

echo "Linking SSH files to ~/.ssh..."
mkdir -p "$HOME/.ssh"
ln -sf "$DOTFILES_DIR/ssh/config" "$HOME/.ssh/config"
ln -sf "$DOTFILES_DIR/ssh/yubikey.pub" "$HOME/.ssh/yubikey.pub"

echo "Dotfiles installation complete!"

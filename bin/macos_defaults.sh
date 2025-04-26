#!/bin/sh



# Keyboard
defaults write -g KeyRepeat -int 2
defaults write -g InitialKeyRepeat -int 15
defaults write -g ApplePressAndHoldEnabled -bool false


# Dock
defaults write com.apple.dock autohide -bool true
defaults write com.apple.dock autohide-time-modifier -float 0.5


# Other
defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true

# Windows
defaults write com.apple.dock "mru-spaces" -bool false

### ~/.config/zsh/.zshrc

## Alias
alias brew-update-all="brew update && brew upgrade && brew list --cask | xargs -I {} brew upgrade --force --cask {} && brew cleanup"

alias dotfiles="cd $HOME/Projects/Personal/dotfiles"

alias awslocal="aws --endpoint-url http://localhost:4566"
alias trigger-ci="git commit --allow-empty -m 'Trigger CI/CD - Empty commit'"
alias git-commit-undo="git reset --soft HEAD~1"


## History
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt inc_append_history
setopt hist_reduce_blanks

### Plugins
source "$HOME/.config/zsh/zsh-defer/zsh-defer.plugin.zsh"

## Keybindings
zsh-defer source "$HOME/.config/zsh/keybindings.zsh"

## Syntax highlighting
zsh-defer eval 'source "$HOME/.config/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"'

## Autocomplete
fpath=(~/.stripe ~/.config/zsh/completions $fpath)
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)
export ZSH_AUTOSUGGEST_USE_ASYNC="true"
autoload -U compinit && compinit
zsh-defer eval 'source "$HOME/.config/zsh/zsh-autosuggestions/zsh-autosuggestions.zsh"'

## Private
zsh-defer source "$HOME/.config/zsh/priv.work.zsh"

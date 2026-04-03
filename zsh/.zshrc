### ~/.config/zsh/.zshrc

## Alias
alias brew-update-all="brew update && brew upgrade && brew list --cask | xargs -I {} brew upgrade --force --cask {} && brew cleanup --prune=all"

alias dotfiles="cd $HOME/Projects/Personal/dotfiles"

alias awslocal="aws --endpoint-url http://localhost:4566"
alias git-empty-commit="git commit --allow-empty -m 'Empty commit (intentional)' && git push"
alias git-commit-undo="git reset --soft HEAD~1"

alias :q="exit"

## History
HISTFILE="${XDG_STATE_HOME}/zsh/history"
if [[ ! -d "${HISTFILE:h}" ]]; then
  mkdir -p "${HISTFILE:h}"
fi

HISTSIZE=100000
SAVEHIST=100000

setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_SAVE_NO_DUPS
setopt HIST_IGNORE_SPACE


### Plugins
source "$HOME/.config/zsh/zsh-defer/zsh-defer.plugin.zsh"

## Keybindings
zsh-defer source "$HOME/.config/zsh/keybindings.zsh"

## Syntax highlighting
zsh-defer eval 'source "$HOME/.config/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"'

## Autocomplete
fpath=(~/.stripe ~/.config/zsh/completions $fpath)
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)
autoload -U compinit && compinit
zsh-defer eval 'source "$HOME/.config/zsh/zsh-autosuggestions/zsh-autosuggestions.zsh"'

## Private
zsh-defer source "$HOME/.config/zsh/priv.work.zsh"

## GREP
export GREP_OPTIONS='--color=always'

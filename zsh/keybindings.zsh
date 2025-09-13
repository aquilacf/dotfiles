bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word

autoload -U select-word-style
select-word-style normal

subword-forward() {
    local WORDCHARS_SAVE=$WORDCHARS
    WORDCHARS=''
    zle forward-word
    WORDCHARS=$WORDCHARS_SAVE
}

subword-backward() {
    local WORDCHARS_SAVE=$WORDCHARS
    WORDCHARS=''
    zle backward-word
    WORDCHARS=$WORDCHARS_SAVE
}

zle -N subword-forward
zle -N subword-backward

bindkey "^[[1;3D" subword-backward
bindkey "^[[1;3C" subword-forward

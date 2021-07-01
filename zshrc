# support dumb terminal
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

export TERM=xterm-256color

alias l='lsd -l'
alias ll='l'
alias la='lsd -a'
alias lla='lsd -la'
alias lt='lsd --tree'

# kill word
autoload -U select-word-style
select-word-style bash

# python version management
if command -v pyenv 1>/dev/null 2>&1
then
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi

# Work computer setup
if [[ $(hostname) = '9281' ]]
then
  source "/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc"
  source "/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc"
fi

# Autosuggestions
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

: ${ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=2'}

bindkey 'C-f' autosuggest-accept

# Up and down arrow search
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search # Up
bindkey "^[[B" down-line-or-beginning-search # Down

 # Shell only exists after the 10th consecutive Ctrl-d
set -o ignoreeof

# Setup vterm
vterm_printf() {
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# Main prompt
PROMPT='%(?.%F{green}√.%F{red}?%?)%f %B%F{240}%2~%f%b %# '

if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]
then
  exec startx
# elif [ -z $EMACS ] && [ -z $TMUX ]
# then
#   tmux attach || tmux
fi

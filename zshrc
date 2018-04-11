# oh my zsh config
export ZSH=${HOME}/.oh-my-zsh
export ZSH_THEME="risto"
CASE_SENSITIVE="true"
HYPHEN_INSENSITIVE="true"
COMPLETION_WAITING_DOTS="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"
HIST_STAMPS="yyyy-mm-dd"
plugins=(git)
source $ZSH/oh-my-zsh.sh

# emacs
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"           # $EDITOR should open in terminal
export VISUAL="emacsclient -c -a emacs"  # $VISUAL should open a GUI

alias emax="emacsclient -t"
alias emacsc="emacsclient -c -a emacs"

# use zsh on remote machine
zssh() {
  if [ "$2" = "sync" ]
  then
    rsync -rv --delete --exclude='.git/*' ~/.oh-my-zsh/ ${1}:.oh-my-zsh/
    scp ~/.zshrc ${1}:
    scp ~/.zshenv ${1}:
  fi
  ssh -t $1 "/bin/zsh"
}

# hv env setup
if [[ "$HOME" = "/Users/musifer" ]]
then
  if [[ $(docker-machine ls | grep hv | grep Running) != "" ]]
  then
    echo "Setting up environment for machine 'hv'"
    eval $(docker-machine env hv)
  fi
fi

# config ssh agent
function sshagent_findsockets {
  find /tmp -uid $(id -u) -type s -name agent.\* 2>/dev/null
}

function sshagent_testsocket {
  if [ ! -x "$(which ssh-add)" ] ; then
    echo "ssh-add is not available; agent testing aborted"
    return 1
  fi

  if [ X"$1" != X ] ; then
    export SSH_AUTH_SOCK=$1
  fi

  if [ X"$SSH_AUTH_SOCK" = X ] ; then
    return 2
  fi

  if [ -S $SSH_AUTH_SOCK ] ; then
    ssh-add -l > /dev/null
    if [ $? = 2 ] ; then
      echo "Socket $SSH_AUTH_SOCK is dead!  Deleting!"
      rm -f $SSH_AUTH_SOCK
      return 4
    else
      echo "Found ssh-agent $SSH_AUTH_SOCK"
      return 0
    fi
  else
    echo "$SSH_AUTH_SOCK is not a socket!"
    return 3
  fi
}

function sshagent_init {
  NEW=$1

  # ssh agent sockets can be attached to a ssh daemon process or an
  # ssh-agent process.

  AGENTFOUND=0

  # Attempt to find and use the ssh-agent in the current environment
  if sshagent_testsocket ; then AGENTFOUND=1 ; fi

  # If there is no agent in the environment, search /tmp for
  # possible agents to reuse before starting a fresh ssh-agent
  # process.
  if [ $AGENTFOUND = 0 ] ; then
    for agentsocket in $(sshagent_findsockets) ; do
      if [ $AGENTFOUND != 0 ] ; then break ; fi
      if sshagent_testsocket $agentsocket ; then AGENTFOUND=1 ; fi
    done
  fi

  # If at this point we still haven't located an agent, it's time to
  # start a new one
  if [ $AGENTFOUND = 0 ] ; then
    eval `ssh-agent`
  fi

  # Clean up
  unset AGENTFOUND
  unset agentsocket

  # if new, we need to add keys
  if [[ -n "$NEW" ]]
  then
    ssh-add
    ssh-add ~/.ssh/emr_deployer
  fi

  # Finally, show what keys are currently in the agent
  ssh-add -l
}

alias sa="sshagent_init"

#
# Main prompt
#

# local host_name="%{$fg[cyan]%}musifer"
local path_string="%{$fg[yellow]%}%~"
local prompt_string="»"

# Make prompt_string red if the previous command failed.
local return_status="%(?:%{$fg[blue]%}$prompt_string:%{$fg[red]%}$prompt_string)"

PROMPT='${path_string} ${return_status} %{$reset_color%}'

# oh-my-zsh $(git_prompt_info) puts 'dirty' behind branch
git_custom_prompt() {
  # branch name (.oh-my-zsh/plugins/git/git.plugin.zsh)
  local branch=$(current_branch)
  if [ -n "$branch" ]; then
    # parse_git_dirty echoes PROMPT_DIRTY or PROMPT_CLEAN (.oh-my-zsh/lib/git.zsh)
    echo "$(parse_git_dirty)$ZSH_THEME_GIT_PROMPT_PREFIX$branch$ZSH_THEME_GIT_PROMPT_SUFFIX"
  fi
}

RPROMPT='$(git_custom_prompt) $(date "+%X")'

# # start tmux
# if ! { [ "$TERM" = "screen" ] && [ -n "$TMUX" ]; }
# then
#   tmux attach || tmux new
# fi

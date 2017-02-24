export PATH=$HOME/bin:/usr/local/bin:$PATH

export ZSH=${HOME}/.oh-my-zsh

export ZSH_THEME="risto"

CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# spark setup
# note: requires py4j
#   $ pip install py4j
export SPARK_HOME=~/src/apache/spark-2.0.1-bin-hadoop2.7
export PYTHONPATH=$SPARK_HOME/python/build
export PYTHONPATH=$SPARK_HOME/python/:$PYTHONPATH

# virtualenvwrapper setup
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/src
source /usr/local/bin/virtualenvwrapper.sh

# Preferred editor for local and remote sessions
# export EDITOR='emacs -nw'

# use zsh on remote machine
zssh() {
  if [ "$2" = "tar" ]
  then
    tar c -C${HOME} .oh-my-zsh .zshrc .emacs.d | ssh $1 'tar mx -C${HOME}' 
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

# start tmux
if ! { [ "$TERM" = "screen" ] && [ -n "$TMUX" ]; }
then
  tmux attach || tmux new
fi

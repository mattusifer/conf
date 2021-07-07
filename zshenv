export PATH=$HOME/bin:/usr/local/bin:$PATH
export EDITOR=emacsclient

# History
HISTSIZE=1000000000
HISTFILE=~/.zsh_history
SAVEHIST=1000000000
HISTDUP=erase
setopt appendhistory
setopt sharehistory
setopt incappendhistory

# pyenv for python environment handling
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"

# Work computer setup
if [[ $(hostname) = 'matts-mbp' ]]
then
  export JAVA_HOME=$(/usr/libexec/java_home -v11)
fi

# Fix psycopg2 compilation errors on osx
export LDFLAGS=$(pg_config --ldflags)

# spark
# note: requires py4j
#   $ pip install py4j
export SPARK_HOME=~/src/apache/spark-3.1.1-bin-hadoop2.7
export PYTHONPATH=$SPARK_HOME/python/build
export PYTHONPATH=$SPARK_HOME/python/:$PYTHONPATH
export PYTHONPATH=$SPARK_HOME/python/lib:$PYTHONPATH
export PYTHONPATH=$SPARK_HOME/python/lib/py4j-0.10.4-src.zip:$PYTHONPATH

# go
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin

# rust
export PATH=$PATH:~/.cargo/bin
export RUST_BACKTRACE=1

# node
export PATH=$PATH:/usr/local/Cellar/node/12.4.0/bin

# ruby
export PATH=$PATH:$HOME/.local/share/gem/ruby/3.0.0/bin

# virtualenv setup
export WORKON_HOME=$HOME/.virtualenvs

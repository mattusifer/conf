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

# jenv for java environment handling
export PATH=$HOME/.jenv/bin:$PATH

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

# settings specific to my work computer (mac)
if [ $(uname) = "Darwin" ]
then
  source ~/.blackfynn/env_vars || echo "Env vars file not found!"
fi

# blackfynn envs
# export BLACKFYNN_LOG_LEVEL="DEBUG"

# virtualenv setup
export WORKON_HOME=$HOME/.virtualenvs

# export AWS_ACCESS_KEY_ID=$(grep aws_access_key_id ~/.aws/credentials | awk '{print $3}' || echo '')
# export AWS_SECRET_ACCESS_KEY=$(grep aws_secret_access_key ~/.aws/credentials | awk '{print $3}' || echo '')

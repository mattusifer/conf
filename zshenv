export PATH=$HOME/bin:/usr/local/bin:$PATH

# java
if [ $(uname) = "Darwin" ]
then
  export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_121.jdk/Contents/Home/jre
  export JDK_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_121.jdk/Contents/Home
else
  export JAVA_HOME=/usr/lib/jvm/java-8-openjdk/jre
  export JDK_HOME=/usr/lib/jvm/java-8-openjdk
fi

# spark
# note: requires py4j
#   $ pip install py4j
export SPARK_HOME=~/src/apache/spark-2.4.3-bin-hadoop2.7
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

# settings specific to my work computer (mac)
if [ $(uname) = "Darwin" ]
then
  source ~/.blackfynn/env_vars || echo "Env vars file not found!"
fi

# blackfynn envs
# export BLACKFYNN_LOG_LEVEL="DEBUG"

# virtualenvwrapper setup (if it exists)
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/src
source /usr/local/bin/virtualenvwrapper.sh 2> /dev/null       \
  || source /usr/local/bin/virtualenvwrapper.sh 2> /dev/null  \
  || true

# export AWS_ACCESS_KEY_ID=$(grep aws_access_key_id ~/.aws/credentials | awk '{print $3}' || echo '')
# export AWS_SECRET_ACCESS_KEY=$(grep aws_secret_access_key ~/.aws/credentials | awk '{print $3}' || echo '')

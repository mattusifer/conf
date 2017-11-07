export PATH=$HOME/bin:/usr/local/bin:$PATH

# java
if [ $(uname) = "Darwin" ]
then
  export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_102.jdk/Contents/Home/jre
  export JDK_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_102.jdk/Contents/Home
else
  export JAVA_HOME=/usr/lib/jvm/java-8-openjdk/jre
  export JDK_HOME=/usr/lib/jvm/java-8-openjdk
fi

# spark setup
# note: requires py4j
#   $ pip install py4j
export SPARK_HOME=~/src/apache/spark-2.0.1-bin-hadoop2.7
export PYTHONPATH=$SPARK_HOME/python/build
export PYTHONPATH=$SPARK_HOME/python/:$PYTHONPATH
export PYTHONPATH=$SPARK_HOME/python/lib:$PYTHONPATH
export PYTHONPATH=$SPARK_HOME/python/lib/py4j-0.10.3-src.zip:$PYTHONPATH

# add hv code to python path
if [ $(uname) = "Darwin" ]
then
  export PYTHONPATH=$HOME/src/hv/dewey:$PYTHONPATH
fi

# virtualenvwrapper setup (if it exists)
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/src
source /usr/local/bin/virtualenvwrapper.sh 2> /dev/null       \
  || source /usr/local/bin/virtualenvwrapper.sh 2> /dev/null  \
  || true

export AWS_ACCESS_KEY_ID=$(grep aws_access_key_id ~/.aws/credentials | awk '{print $3}' || echo '')
export AWS_SECRET_ACCESS_KEY=$(grep aws_secret_access_key ~/.aws/credentials | awk '{print $3}' || echo '')

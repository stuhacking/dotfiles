
if [ "$(uname)" == "Linux" ]
then
    export JAVA_HOME=${HOME}/jvm/jdk1.7.0_76
    export PATH=$PATH:${JAVA_HOME}/bin
fi

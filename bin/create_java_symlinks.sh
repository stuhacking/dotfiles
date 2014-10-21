#! /bin/bash

# Only do this on Linux
if [ "$(uname)" == "Linux" ]
then
    for i in `find $JAVA_HOME/bin -type f -executable -perm -u+x`
    do
        B=$(basename $i)
        sudo ln -sf $i /usr/bin/$B
    done
fi

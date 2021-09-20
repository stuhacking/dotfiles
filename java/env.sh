# Try to figure out java home dir from whichever java binary
# is being used.

JAVA_BIN=$(realpath `which java`)

if [[ ! -f "$JAVA_BIN" ]]; then
    echo "dotfiles/java: Couldn't find java binary, not setting JAVA_HOME."
    exit
fi

JAVA_HOME=$(echo "$JAVA_BIN" | sed 's:/bin/java::')

if [[ ! -d "$JAVA_HOME" ]]; then
    echo "No such directory: $JAVA_HOME"
fi

export JAVA_HOME

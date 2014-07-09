HASKELL_BIN_PATH=~/.cabal/bin

if [ "Darwin" == `uname -s` ]; then
    HASKELL_BIN_PATH=$HASKELL_BIN_PATH:~/Library/Haskell/bin
fi


export PATH=$PATH:$HASKELL_BIN_PATH


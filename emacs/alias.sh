# System aliases for Emacs

if [ "Darwin" == `uname` ]
then
    alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
fi

export ALTERNATE_EDITOR=""

alias e='emacsclient -c -a emacs'
alias et='emacsclient -t'

alias vi='emacsclient -t'
alias vim='emacsclient -t'

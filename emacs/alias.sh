# System aliases for Emacs

if [ "Darwin" == `uname` ]
then
    alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
fi

alias e='emacsclient -t'
alias em='emacsclient -c'
alias ec='emacsclient'

alias vi='emacsclient -t'
alias vim='emacsclient -t'

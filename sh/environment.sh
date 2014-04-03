export PATH=$PATH:/usr/local/bin:~/bin:$DOTFILES/bin

# Emacs if possible
which emacs &> /dev/null
if [ x0 == x$? ]; then
    export EDITOR='emacs -nw'
    export VISUAL='emacs'
fi

if [ 'Darwin' == `uname -s` ]; then
    GIT_COMLETIONS=/Applications/Xcode.app/Contents/Developer/usr/share/git-core/git-completion.bash
    if [ -f $GIT_COMLETIONS ]; then
        . $GIT_COMLETIONS
    fi
    
    source /Applications/Xcode.app/Contents/Developer/usr/share/git-core/git-prompt.sh
fi

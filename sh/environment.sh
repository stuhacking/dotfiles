export PATH=$PATH:/usr/local/bin:~/bin:$DOTFILES/bin

# Emacs if possible
which emacs &> /dev/null
if [ x0 == x$? ]; then
    export EDITOR='emacs -nw'
    export VISUAL='emacs'
fi


export PATH=$PATH:~/bin:$DOTFILES/bin

# Emacs if possible
which emacs &> /dev/null
if [ x0 == x$? ]; then
    export EDITOR='emacs -nw'
    export VISUAL='emacs'
fi


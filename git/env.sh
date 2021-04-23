export GIT_CUSTOM_DIR=$DOTFILES/git

__git_ps1 > /dev/null 2> /dev/null
if [ "X$?" != "X0" ]; then
    # Try to find git enhancement scripts.
    GIT_EXTRAS_DIR=/usr/local/git/contrib

    if [ -d "$GIT_EXTRAS_DIR" ]; then
        export PATH=$GIT_EXTRAS_DIR/bin:$PATH
        . $GIT_EXTRAS_DIR/contrib/completion/git-prompt.sh
        . $GIT_EXTRAS_DIR/contrib/completion/git-completion.bash
    else
        # Probably work laptop.
        . ${HOME}/bin/git-prompt.sh
        . ${HOME}/bin/git-completion.sh
    fi
fi

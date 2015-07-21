export GIT_CUSTOM_DIR=$DOTFILES/git

GIT_INSTALL_DIR=/usr/local/git

if [ -d "$GIT_INSTALL_DIR" ]; then
    export PATH=$GIT_INSTALL_DIR/bin:$PATH
    . $GIT_INSTALL_DIR/contrib/completion/git-prompt.sh
    . $GIT_INSTALL_DIR/contrib/completion/git-completion.bash
fi

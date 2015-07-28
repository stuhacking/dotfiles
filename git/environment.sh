export GIT_CUSTOM_DIR=$DOTFILES/git

GIT_INSTALL_DIR=/usr/local/git

if [ -d "$GIT_INSTALL_DIR" ]; then
    export PATH=$GIT_INSTALL_DIR/bin:$PATH
    . $GIT_INSTALL_DIR/contrib/completion/git-prompt.sh
    . $GIT_INSTALL_DIR/contrib/completion/git-completion.bash

elif
    # Because Oracle Linux is old, stick the scripts in
    # a local folder.
    __git_ps1 > /dev/null 2> /dev/null
    if [ "$?" != "0" ]
    then
        source ~/bin/git-prompt.sh
        source ~/bin/git-completion.sh
    fi
fi

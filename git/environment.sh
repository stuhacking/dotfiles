export GIT_CUSTOM_DIR=$DOTFILES/git

function git-current-branch () {
    git branch | grep '*' | cut -d ' ' -f 2
}


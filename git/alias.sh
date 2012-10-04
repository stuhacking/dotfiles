# Git Aliases

function git-current-branch () {
    git branch | grep '*' | cut -d ' ' -f 2
}

alias gst='git status'
alias gdiff='git diff'

alias gl='git log --graph --color=auto'
alias glsmall='git log --graph --color=auto --pretty=oneline --abbrev-commit'

alias gcl='git clone'
alias gco='git commit'

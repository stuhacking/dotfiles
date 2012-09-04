#! /bin/zsh
#
# ZShell aliases

# Directory listing
if [[ $(uname) == "Darwin" ]]
then
	# Good start - thanks Apple.
	alias ls='ls -G'
else
	alias ls='ls --color=auto'
fi

alias ll='ls -l'
alias la='ls -a'
alias lf='ls -F'

alias lla='ls -la'
alias llf='ls -lF'
alias laf='ls -lFa'

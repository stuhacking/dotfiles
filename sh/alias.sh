# Global Aliases

alias reload-sh='source ~/.bashrc'
alias reload-x='sh /etc/X11/Xsession.d/45x11-custom-xrandr'

# Directory listing
if [ "$(uname)" == "Darwin" ]
then
	# Good start - thanks Apple.
	alias ls='ls -G -h'
else
	alias ls='ls --color=auto -h'
fi

alias ll='ls -l'
alias la='ls -a'
alias lf='ls -F'

alias lla='ls -la'
alias llf='ls -lF'
alias laf='ls -lFa'

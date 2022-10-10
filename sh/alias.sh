# Global Aliases

alias reload-sh='source ~/.bashrc'
alias reload-x='sh /etc/X11/Xsession.d/45x11-custom-xrandr'

# Directory listing
if [ "$(uname)" == "Darwin" ]
then
	# Good start - thanks Apple.
	alias lsc='ls -G -h'
else
	alias lsc='ls --color=auto -h'
fi

alias ll='lsc -l'
alias la='lsc -a'
alias lf='lsc -F'

alias lla='lsc -la'
alias llf='lsc -lF'
alias laf='lsc -lFa'

alias ff='find . -type f'

alias cgrep='grep --color=auto'
alias ggrep='cgrep -Hn'
alias xgrep='xargs ggrep'

alias whereami='echo $HOSTNAME'


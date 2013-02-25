
# On OS X, try to use the latest installed emacs version
if [ 'Darwin' == `uname -s` ]; then
    if [ -e /Applications/Emacs.app ]; then
	alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw -q'
	alias memacs='open -a /Applications/Emacs.app'
    fi
fi


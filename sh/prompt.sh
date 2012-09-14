# Prompt Config
txtred='\e[0;31m'
txtgrn='\e[0;32m'
txtylw='\e[0;33m'
txtblu='\e[0;36m'
txtwht='\e[0;37m'
txtrst='\e[0m'

function make_prompt ()
{
    if [ $? != 0 ]
    then
        PS1="\[$txtred\]\#\[$txtrst\] \[$txtblu\]\W\[$txtrst\]\$ "
        PS2="\[$txtred\]>\[$txtrst\] "
    else
        PS1="\[$txtgrn\]\#\[$txtrst\] \[$txtblu\]\W\[$txtrst\]\$ "
        PS2="\[$txtgrn\]>\[$txtrst\] "
    fi
}

PROMPT_COMMAND=make_prompt

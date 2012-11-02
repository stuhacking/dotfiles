
function make_git_prompt ()
{
    git status 2> /dev/null > /dev/null
    if [ $? == 0 ]
    then
        echo "[$(git-current-branch)]"
    else
        echo ""
    fi
}

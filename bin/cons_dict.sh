#! /bin/sh

# Consolidate Dictionaries
# The following dictionaries are used:
#  ~/.aspell.en.pws - ASpell's personal word list
#  coder.dic - Used by IDEA and Eclipse
#
# The master word list is 'words'. This script consolidates
# new additions from either into the master list, then
# updates all dictionaries.

{
pushd $DOTFILES/dict
 tail -n+2 aspell.en.pws.symlink >> /tmp/aspell-$$
 cat coder.dic >> /tmp/coder-$$
 cat /tmp/aspell-$$ /tmp/coder-$$ words | sort | uniq > /tmp/words-$$
 cat /tmp/words-$$ > words
 rm /tmp/words-$$ /tmp/aspell-$$ /tmp/coder-$$

 cat words > coder.dic

 LINES=`cat words | wc -l`
 echo "personal_ws-1.1 en ${LINES}" > aspell.en.pws.symlink
 cat words >> aspell.en.pws.symlink
popd
} &> /dev/null

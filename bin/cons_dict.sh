#!/bin/bash --noprofile

# Consolidate Dictionaries
# The following dictionaries are used:
#  ~/.aspell.en.pws - ASpell's personal word list
#  coder.dic - Used by IDEA and Eclipse
#
# The master word list is 'words'. This script consolidates
# new additions from either into the master list, then
# updates all dictionaries.

if [[ -z "$DOTFILES" ]]; then
    echo 'DOTFILES variable is empty in this session.' &1>2
    exit 1
fi

pushd $DOTFILES/dict
 echo "Backing up ASpell and Eclipse dictionaries."
 tail -n+2 aspell.en.pws.symlink >> /tmp/aspell-$$
 cat coder.dic >> /tmp/coder-$$

 # Cat the contents of the child and master dictionaries, uniqify
 # and recombine.
 echo "Combining words"
 cat /tmp/aspell-$$ /tmp/coder-$$ words | sort | uniq > /tmp/words-$$
 cat /tmp/words-$$ > words
 rm /tmp/words-$$ /tmp/aspell-$$ /tmp/coder-$$

 echo "Recreating ASpell and Eclipse dictionaries."
 cat words > coder.dic

 nlines=`cat words | wc -l`
 echo "Updated dictionary contains ${nlines} entries."
 echo "personal_ws-1.1 en ${nlines}" > aspell.en.pws.symlink
 cat words >> aspell.en.pws.symlink
popd

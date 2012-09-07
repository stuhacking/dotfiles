Dotfiles
========

.configuration files for Unix/Linux.

Uses a perl installer (WIP) to avoid the Ruby/Rake dependency.

Files with a .sh extension will be sourced when a new bash 
shell is started. They can be located with the programs to 
which they relate.

Files with a .symlink extension will be linked to an entry
in your home directory (~/).

Set the DOTFILES variable in sh/bashrc.symlink to the path of
the dotfiles project directory.

Contents
--------

1) sh/ - Global Shell Configuration

2) vim/ - Vim is good.

3) emacs/ - Emacs is good.
            (Emacs site-lisp/ directory is omitted due to size.)

4) Readme.md - This file

5) dotfiles.pl - installer script
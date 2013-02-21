Dotfiles
========

These are my .configuration files for Unix/Linux. If you find anything of
use here then please take it, modify it, share it and all that good stuff!

This version uses a perl installer to avoid the Ruby/Rake dependency.

Installation
---------------

Just clone the git repository, cd into the directory and run:

    dotfiles$ perl dotfiles.pl install

Files with a .sh extension will be sourced when a new bash 
shell is started. They can be colocated with the programs they
customize.

Files with a .symlink extension will be linked to an entry
in your home directory (~/).

Set the DOTFILES variable in sh/bashrc.symlink to the path of
the dotfiles project directory.

After cloning the repo for the first time, don't forget to update the
git submodules.

    dotfiles$ git submodule init
    dotfiles$ git submodule update

Also ensure that emacs packages are installed, or disable loading
them in the emacs config.

Contents
--------

Most subdirectories simply contain some basic environment setup
like setting PATH entries. The more significant files are:

1) sh/ - Global Shell Configuration

2) vim/ - Vim is good.

3) emacs/ - Emacs is good.
            (Emacs site-lisp/ directory is omitted due to size. Just use ELPA now instead, it's awesome!)

4) Readme.md - This file

5) dotfiles.pl - installer script

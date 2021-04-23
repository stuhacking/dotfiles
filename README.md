DotFiles
========

System configuration, aliases and scripts. To install, run the dotfiles.pl script. Any file or
directory having a {{.symlink}} suffix will have a symbolic link added to
${HOME}. The dotfilename will be the original filename minus the '.symlink'
suffix and with a '.' prefix.

In addition, the .bashrc file will be amended with some convenience features:

* A DOTFILES environment variable will be defined in bash sessions which is the
  path to the DOTFILES installation.
* $DOTFILES/bin will be added to the user $PATH.
* Any script named 'env.sh' or 'alias.sh' will be sourced on starting a bash
  session so preferences can be defined per module.
* A fancy (git enabled) command prompt is defined in sh/prompt.sh which will
  also be loaded in new sessions
* If you change or add an env.sh or alias.sh file, the function 'reload-sh' will
  re source bashrc.

Contents
--------

* bin/   - Scripts, etc.
* dict/  - Word lists in vanilla, aspell and eclipse formats.
* sh/    - General shell configuration.
* emacs/ - Text editing (and much more).
* git/   - Git config, global git ignore, convenient aliases.

Smaller modules: nethack, node, python, cmake, vim mostly just setting up the
PATH and associated environment variables.

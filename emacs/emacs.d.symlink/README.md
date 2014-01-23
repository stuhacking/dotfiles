.emacs.d
========

My Emacs Init Dir.
Intended for use with GNU Emacs running on Mac OSX (Home) and 
Linux (Work). Updated for Emacs version 24.3. Using earlier Emacs 
versions probably requires modification.

Contents
--------

* init.el - primary customisations loaded at emacs startup.
* custom.el - custom-set-variables, custom-set-faces
* init/
 * init-kbd     - Global key bindings
 * init-win     - Windows specific utilities
 * init-lisp    - Lisp customisations
 * init-py      - Python Customization
 * init-haskell - Haskell customisations
* lisp/
 * number-to-word - Utility for converting numbers to human readable string. 
 * out-of-time - additional date utils
 * perl-helper - utilities for perl scripting
 * window-state - save and load window layouts
 * wordcount - the obligatory, personal word count module

Dependencies
------------

The site-lisp/ directory was previously used for third party libraries. Now, the package manager (ELPA) will place libraries in the 'elpa/' directory. The following packages are recommended:

* rainbow-delimiters - colorise matching parens in lisp modes.
* paredit            - auto-balance parens in lisp modes.
* slime              - Common Lisp REPL & Debugging utilities.
* cider              - Clojure REPL.
* rainbow-mode       - Colorize #hex; strings in CSS and *HTML modes.
* typing-of-emacs    - Fun speed typing game.

Additional data directories org/, asave/, and backup/ are also omitted.

License
-------

 Copyright (C) 2011 by Stuart Hacking

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

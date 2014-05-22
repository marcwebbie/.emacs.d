# marcwebbie emacs

This is my emacs configuration files. If you want to use them, the
best thing is probably to fork the project at Github
(<http://github.com/marcwebbie/emacs>) and then make your changes to that
branch.

## Installation

First of all make sure you have installed Emacs version 24 or higher.

Fetch the emacs source files:

    git clone git://github.com/marcwebbie/emacs.git ~/.emacs.d

Install all ELPA packages (make sure you have [Cask](https://github.com/cask/cask) installed):

    cd ~/.emacs.d
    cask

## Using Xdefaults for setting fonts in linux

    cp ~/.emacs.d/.Xdefaults ~/.Xdefaults
    xrdb -merge ~/.Xdefaults

## Setting emacs to run in daemon mode

    # OSX
    alias es='/Applications/Emacs.app/Contents/MacOS/Emacs --daemon'
    alias emacs='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c -n $*'
    export EDITOR='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c'

## License

This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.

In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

For more information, please refer to <http://unlicense.org>

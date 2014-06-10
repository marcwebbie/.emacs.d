# My emacs config

This is my emacs configuration files. If you want to use them, the
best thing is probably to fork the project at Github
(<http://github.com/marcwebbie/emacs>) and then make your changes to that
branch. This config is based on
[rejeep's](https://github.com/rejeep/emacs) and [magnar's](https://github.com/magnars/.emacs.d) configs.

## Installing emacs

First of all make sure you have installed Emacs version 24 or higher.

### OSX

I use Cocoa Emacs, installed like this:

    brew install emacs --cocoa --srgb

### Ubuntu [source](http://askubuntu.com/questions/297170)

    sudo add-apt-repository ppa:cassou/emacs
    sudo apt-get update

For emacs-snapshot:

    sudo apt-get install emacs-snapshot-el emacs-snapshot-gtk emacs-snapshot

Or, for emacs24 (i.e. 24.3, stable):

    sudo apt-get install emacs24 emacs24-el emacs24-common-non-dfsg

### Compile emacs 24 from source on Red Hat 6

    curl -fsSkl https://raw.githubusercontent.com/marcwebbie/emacs/master/scripts/compile_emacs24_rh6.sh | sh

### Fetch the config files:

    git clone git://github.com/marcwebbie/emacs.git ~/.emacs.d

### Install all ELPA packages (make sure you have [Cask](https://github.com/cask/cask) installed):

    ~/.emacs.d/scripts/install.sh

Or,

    cd ~/.emacs.d
    cask

## Tips for using these emacs settings

If you want to use my settings straight out of the box, here are some things to note:

 * I recommend starting with a blank emacs +
   [Technomancy's better-defaults package](https://github.com/technomancy/better-defaults),
   and then dig through this repo for useful nuggets, instead of forking it directly.

 * Start by reading up on all the cool stuff in key-bindings.el.

 * Autocomplete using hippie-expand with `M-/` (autocomplete entire lines with `C-:`)

 * On a mac, the Meta key `M` is bound to Command.

 * I recommend rebinding Caps Lock to Ctrl and use that instead of the often badly placed Ctrl-key.

 * Watch [emacsrocks.com](http://emacsrocks.com)

## Optional env setup

### Using Xdefaults for setting fonts in linux

    echo "Emacs.font: Inconsolata-12" >> ~/.Xdefaults
    xrdb -merge ~/.Xdefaults

### Setting emacs to run in daemon mode

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

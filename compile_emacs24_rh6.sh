# install needed libraries
sudo yum install texinfo libXpm-devel giflib-devel libtiff-devel libotf-devel

# Compile autoconf
cd /tmp
curl -L http://ftp.gnu.org/gnu/autoconf/autoconf-2.69.tar.gz | tar zx
cd autoconf-2.69/
./configure && make && sudo make install
cd ..

# compile emacs 24.3
cd /tmp
curl -L https://github.com/mirrors/emacs/archive/emacs-24.3.91.tar.gz | tar zx
cd emacs-emacs-24.3.91
./autogen.sh && ./configure --with-jpeg=no && make bootstrap && sudo make install
cd ..

# Setup config

## Install cask
#curl -fsSkL https://raw.github.com/cask/cask/master/go | Python
git clone https://github.com/cask/cask.git ~/.cask

# install config
git clone git://github.com/marcwebbie/emacs.git ~/.emacs.d
cd ~/.emacs.d && ~/.cask/bin/cask

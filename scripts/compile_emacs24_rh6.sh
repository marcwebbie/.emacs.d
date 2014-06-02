# install needed libraries
sudo yum install texinfo libXpm-devel giflib-devel libtiff-devel libotf-devel libjpeg-devel

# compile autoconf
cd /tmp
curl -L http://ftp.gnu.org/gnu/autoconf/autoconf-2.69.tar.gz | tar zx
cd autoconf-2.69/
./configure && make && sudo make install
cd ..
rm -rf autoconf-2.69

# compile emacs 24.3
cd /tmp
curl -L http://mirrors.syringanetworks.net/gnu/emacs/emacs-24.3.tar.gz
cd emacs-24.3/
./autogen.sh && ./configure && make bootstrap && sudo make install
cd ..
rm -rf emacs-24.3

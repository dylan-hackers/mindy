#!/bin/sh
# Run this to generate all the initial makefiles, etc.

DIE=0

(autoconf --version) < /dev/null > /dev/null 2>&1 || {
        echo
        echo "You must have autoconf installed to compile Gwydion Dylan."
        echo "Download the appropriate package for your distribution,"
        echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/"
        DIE=1
}

#(automake --version) < /dev/null > /dev/null 2>&1 || {
#        echo
#        echo "You must have automake installed to compile gg_sim."
#        echo "Get ftp://ftp.gnu.org/pub/gnu/automake-1.3.tar.gz"
#        echo "(or a newer version if it is available)"
#        DIE=1
#}

if test "$DIE" -eq 1; then
        exit 1
fi

(test -d d2c && test -d mindy) || {
        echo "You must run this script in the src/ directory"
        exit 1
}

if test -z "$*"; then
        echo "I am going to run ./configure with no arguments - if you wish "
        echo "to pass any to it, please specify them on the $0 command line."
fi

echo processing...

#aclocal $ACLOCAL_FLAGS
#automake
autoheader
autoconf

./configure "$@"

echo 
echo "Now type 'make' to compile Gwydion Dylan."

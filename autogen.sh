#!/bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
if test -z "$srcdir"; then srcdir=.; fi

DIE=0

(autoconf --version && autoheader --version) < /dev/null > /dev/null 2>&1 || {
        echo
        echo "You must have autoconf installed to compile Gwydion Dylan."
        echo "Download the appropriate package for your distribution,"
        echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/"
        DIE=1
}

(libtool --version) < /dev/null > /dev/null 2>&1 || {
        echo
        echo "You must have libtool installed to compile Gwydion Dylan."
        echo "Get ftp://ftp.gnu.org/pub/gnu/libtool/libtool-1.3.2.tar.gz"
        echo "(or a newer version if it is available)"
        DIE=1
}

if test "$DIE" -eq 1; then
        exit 1
fi

(test -d $srcdir/d2c && test -d $srcdir/mindy) || {
        echo "I can't find the Gwydion Dylan source directory in $srcdir"
        exit 1
}

echo processing...

( cd $srcdir
  aclocal $ACLOCAL_FLAGS
  #automake
  libtoolize --force --copy
  autoheader
  autoconf )

if test -z "$*"; then
        echo "I am going to run ./configure with no arguments - if you wish "
        echo "to pass any to it, please specify them on the $0 command line."
fi

rm -f config.cache
$srcdir/configure "$@"

echo 
echo "Now type 'make' to compile Gwydion Dylan."

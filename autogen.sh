#!/bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
if test -z "$srcdir"; then srcdir=.; fi

$srcdir/real-autogen.sh

if test -z "$*"; then
        echo "I am going to run ./configure with no arguments - if you wish "
        echo "to pass any to it, please specify them on the $0 command line."
fi

rm -rf config.cache autom4te-*.cache
$srcdir/configure "$@"


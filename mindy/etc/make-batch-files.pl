#!perl

# Usage:
#    perl make-batch-files.pl

# Creates an MSDOS/WindowsNT batch file called "finish-make.bat"
# that'll run all of the make files.
# This gets around the fact that DOS can't handle
#       for dir in ${SUBDIRS}; do (cd $$dir; ${MAKE});done
# which is used to do recursive makes.

open(OUTPUT, ">finish-make.bat");
print OUTPUT "@echo off";
print OUTPUT "rem This is a batch file to similate recursive makes ",
    "using *real*\n";
print OUTPUT "rem makefiles.  Don't bother hand editing.\n";
print OUTPUT "\n";

open(FIND, "find . -name Makefile -print|");

while (<FIND>) {
    chop;
    tr|/|\\|;   # Translate / to \
    print OUTPUT "make -f ", $_, " %1 %2 %3 %4 %5 %6 %7 %8 %9\n";
}
